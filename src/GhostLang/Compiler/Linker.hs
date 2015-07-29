{-# LANGUAGE OverloadedStrings #-}
module GhostLang.Compiler.Linker
    ( ProcDef
    , ProcMap
    , linkProgram
    , semanticChecks
    , resolve
    , buildProcMap
    , findProcDefs
    ) where

import Data.Maybe (fromMaybe)
import Control.Monad (forM_, foldM)
import Control.Monad.Writer (Writer, execWriter, runWriter, tell)
import Data.List (foldl')
import GhostLang.Types ( Label
                       , ModuleSegment
                       , Value
                       , Declaration (..)
                       , GhostModule (..)
                       , Pattern (..)
                       , Procedure (..)
                       , Program (..)
                       , Operation (..)
                       )
import Text.Printf (printf)
import qualified Data.Map.Strict as Map

type ProcDef a = (ModuleSegment, Procedure a)
type ProcMap a = Map.Map Label [ProcDef a]

-- | Link several ghost modules to a program. Assumes:
-- 1. Non empty set of ghost modules.
-- 2. Exactly one module named "Main".
-- 3. Module "Main" have at least one pattern.
-- 4. No other modules implement any patterns.
linkProgram :: [GhostModule a] -> Either String (Program a)
linkProgram xs = semanticChecks xs >>=
                 resolve           >>=
                 makeProgram

-- | Perform semantic checks of the modules. If everything is ok the
-- same set of modules is returned.
semanticChecks :: [GhostModule a] -> Either String [GhostModule a]
semanticChecks xs = 
    uniqueMainModule xs        >>= 
    mainModuleMustHavePatterns >>=
    otherMustNotHavePatterns

-- | Make sure that there's exactly one module named "Main".
uniqueMainModule :: [GhostModule a] -> Either String [GhostModule a]
uniqueMainModule xs =
    let ys = getMainModules xs
    in case length ys of
         0 -> Left "No module named \"Main\""
         1 -> Right xs
         _ -> Left (ambiguousMain ys)
        where
          ambiguousMain :: [GhostModule a] -> String
          ambiguousMain ys = execWriter $ do
              tell "Ambiguous references of module \"Main\":\n"
              forM_ ys $ \(GhostModule mdecl _ _ _) ->
                  tell $ printf " %s\n" (show $ srcPos mdecl)

-- | Check that the main module implement at least one pattern.
mainModuleMustHavePatterns :: [GhostModule a] -> Either String [GhostModule a]
mainModuleMustHavePatterns xs =
    let ys = getMainModules xs
        ps = foldl' numPatterns 0 ys
    in if ps > 0 then Right xs
       else (Left "Module \"Main\" must have at least one pattern")
        where numPatterns n (GhostModule _ _ ps _) = n + length ps

-- | Other modules are not allowed to implement any patterns.
otherMustNotHavePatterns :: [GhostModule a] -> Either String [GhostModule a]
otherMustNotHavePatterns xs =
    let ys = getNonMainModules xs
    in case runWriter $ scanModules ys of
         (0, _) -> Right xs
         (_, w) -> Left w
        where
          scanModules = foldM g 0
          g acc (GhostModule _ _ ps _) = do
              forM_ ps $ \(Pattern pos _ _ _) ->
                  tell $ printf "Non \"Main\" module pattern: %s\n" (show pos)
              return $ acc + length ps

-- | Main resolver entry.
resolve :: [GhostModule a] -> Either String [GhostModule a]
resolve xs = do
  let procMap = buildProcMap xs
    -- Check if any messages are generated. If not, everything went
    -- ok.
  case runWriter $ resolve' procMap xs of
    (mods, "") -> Right mods
    (_, msg)   -> Left msg
    where 
      resolve' :: ProcMap a -> [GhostModule a] -> Writer String [GhostModule a]
      resolve' procMap = mapM (visitModule procMap)

      visitModule procMap (GhostModule mdecl idecl pts procs) = do
          -- The module name needs to be added to the import set in
          -- order to make the resolved find locally defined
          -- procedures.
          let imports = expName mdecl : map expName idecl
          GhostModule mdecl idecl <$> mapM (visitPattern procMap imports) pts
                                  <*> mapM (visitProc procMap imports) procs

      visitPattern procMap imports (Pattern pos l w ops) =
          Pattern pos l w <$> mapM (resolveOp procMap imports) ops

      visitProc procMap imports (Procedure l ls ops) =
          Procedure l ls <$> mapM (resolveOp procMap imports) ops

-- | The workhorse doing the real resolving.
resolveOp :: ProcMap a -> [ModuleSegment] -> Operation a 
          -> Writer String (Operation a)
resolveOp procMap importDecl unr@(Unresolved pos l params) =
  case findProcDefs l procMap of
    -- No module is defining a procedure with this name.
    []      -> do
      tell $ printf "Procedure %s can not be resolved: %s\n"
                    (show l) (show pos)
      return unr
    -- At least one module is defining a procedure with this
    -- name. Make further checks.
    defMods ->
        case importDecl `intersectProcDefs` defMods of
          -- Module is not importing any modules defining the
          -- procedure. Recommend the user to import one of the
          -- modules in the defMods list.
          [] -> do
            tell $ printf "Procedure %s can not be resoled: %s\n"
                          (show l) (show pos)
            tell "Try import one of:\n"
            forM_ defMods $ \defMod ->
                tell $ printf " %s\n" (show $ fst defMod)
            return unr

          -- There's one unique match for the imported modules that
          -- are exporting the procedure name. If the arity of the
          -- call site values and the procedure's input labels match,
          -- we're done.
          [(_, proc)] -> 
              if params `sameArityAs` proc then return $ Call proc params
              else do
                tell $ printf "Procedure %s wrong number of arguments: %s\n"
                              (show l) (show pos)
                return unr

          -- Ambiguous which procedure definition to take.
          founds -> do
            tell $ printf "Ambiguous of which %s to choose. Defined in:\n"
                          (show l)
            forM_ founds $ \defMod ->
                tell $ printf " %s\n" (show $ fst defMod)
            return unr
      where
        sameArityAs :: [Value] -> Procedure a -> Bool
        sameArityAs xs (Procedure _ ys _) = length xs == length ys

-- Non resolvable operation. Just return the input op.
resolveOp _ _ op = return op

-- | Clean away module stuff and make a program.
makeProgram :: [GhostModule a] -> Either String (Program a)
makeProgram xs =
    case getMainModules xs of
      [GhostModule _ _ pts _] -> Right $ Program pts
      _                       -> Left "Too few/many instances of \"Main\""

-- | Build the procedure map from the list of parsed modules. The key
-- into the map is the procedure's name, and the entry is a list of
-- definitions of that name (if more than one).
buildProcMap :: [GhostModule a] -> ProcMap a
buildProcMap = foldl' visitModule Map.empty
    where
      visitModule :: ProcMap a -> GhostModule a -> ProcMap a
      visitModule acc mod' =
          let (l, ps) = modInfo mod'
          in foldl' (visitProc l) acc ps

      visitProc :: ModuleSegment -> ProcMap a -> Procedure a -> ProcMap a
      visitProc l acc proc = 
          Map.alter (\e -> case e of
                             Just entry -> Just ((l, proc):entry)
                             Nothing    -> Just [(l, proc)]
                    ) (pname proc) acc

      modInfo :: GhostModule a -> (ModuleSegment, [Procedure a])
      modInfo (GhostModule l _ _ ps) = (expName l, ps)

      pname :: Procedure a -> Label
      pname (Procedure l _ _ ) = l

-- | Find all procedure definitions for the given name.
findProcDefs :: Label -> ProcMap a -> [ProcDef a]
findProcDefs k m = fromMaybe [] $ Map.lookup k m

getMainModules :: [GhostModule a] -> [GhostModule a]
getMainModules = filter g
    where g (GhostModule mdecl _ _ _) = "Main" == expName mdecl

getNonMainModules :: [GhostModule a] -> [GhostModule a]
getNonMainModules = filter g
    where g (GhostModule mdecl _ _ _) = "Main" /= expName mdecl

-- | Tailor made intersect of a list of labels and a set of proc defs.
intersectProcDefs :: [ModuleSegment] -> [ProcDef a] -> [ProcDef a]
intersectProcDefs xs = filter inList
    where inList (lbl, _) = lbl `elem` xs

                       
                      
