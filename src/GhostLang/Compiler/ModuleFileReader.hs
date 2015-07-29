{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GhostLang.Compiler.ModuleFileReader
    ( runModuleReader
    , parseGhostModules
    ) where

import Control.Exception (SomeException, try)
import Control.Monad.State (MonadState, StateT, evalStateT, modify', get)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.List (delete, find)
import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Compiler.Grammar (ghostModuleDef)
import GhostLang.Types ( ImportDecl (..) 
                       , GhostModule (..)
                       )
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)
import Text.Printf (printf)
import System.FilePath

type ParseResult = Either ParseError (GhostModule IntrinsicSet)
type PendList = [(FilePath, Bool)]
data Result = Ok | FailedWith String
    deriving Eq

data State = State { baseDir  :: !FilePath
                   , pendList :: ![(FilePath, Bool)]
                   , modList  :: ![GhostModule IntrinsicSet] }

-- | Monad stack for module reading. Just StateT on top of IO.
newtype ModuleReader a = 
    ModuleReader { extrModuleReader :: StateT State IO a }
    deriving (Functor, Applicative, Monad, MonadState State, MonadIO)

-- | Run the ModuleReader monad stack.
runModuleReader :: ModuleReader a -> IO a
runModuleReader act = evalStateT (extrModuleReader act) emptyState

-- | Entry for the parsing of modules parsing.
parseGhostModules :: FilePath 
                  -> ModuleReader (Either String [GhostModule IntrinsicSet])
parseGhostModules path = do
  -- Save the base directory for the input - Main - file. It will be
  -- used as base for all file reads.
  setBaseDir $ takeDirectory path

  -- Add the first pending. Pendings is what make the engine run.
  addPending $ takeFileName path

  result <- parseLoop
  case result of
    Ok             -> do
        mods <- getModList
        return $ Right mods
    FailedWith msg -> return $ Left msg
  
-- | Parse until all pending files are consumed or an error has
-- occurred.
parseLoop :: ModuleReader Result
parseLoop = do
  pending <- nextPending
  case pending of
    Just file -> do
        result <- parseGhostMod file
        if result == Ok then parseLoop
        else return result

    -- No more modules to read, and no errors. Done!
    Nothing   -> return Ok

-- | The parsing workhorse.
parseGhostMod :: FilePath -> ModuleReader Result
parseGhostMod file = do
  -- Combine the filename with the stored base to form a complete
  -- path.
  path <- combineWithBaseDir file

  -- Invoke the parser. The result is Either in two levels, exception
  -- level and parser error level.
  maybeFailed <- liftIO $ tryParse path
  case maybeFailed of
    Right res -> case res of
                   Right m  -> do togglePending file
                                  addModule m
                                  mapM_ addPending $ importSet m
                                  return Ok
                   Left msg -> return $ FailedWith (show msg)
    Left exc  -> return $ FailedWith (printf "Got %s when reading %s" 
                                     (show exc) path)

  where 
    tryParse :: FilePath -> IO (Either SomeException ParseResult)
    tryParse path = try $ parseFromFile ghostModuleDef path

combineWithBaseDir :: FilePath -> ModuleReader FilePath
combineWithBaseDir file = combine <$> (baseDir <$> get) <*> pure file

setBaseDir :: FilePath -> ModuleReader ()
setBaseDir path = modify' $ \s -> s { baseDir = path }

addPending :: FilePath -> ModuleReader ()
addPending file = modify' $ \s -> s { pendList = addPending' file (pendList s) }

nextPending :: ModuleReader (Maybe FilePath)
nextPending = do
  xs <- pendList <$> get
  return $ maybe Nothing (Just . fst) (find snd xs)

togglePending :: FilePath -> ModuleReader ()
togglePending file = 
    modify' $ \s -> s { pendList = togglePending' file (pendList s) }

addModule :: GhostModule IntrinsicSet -> ModuleReader ()
addModule m = modify' $ \s -> s { modList = m : modList s }

getModList :: ModuleReader [GhostModule IntrinsicSet]
getModList = modList <$> get

emptyState :: State
emptyState = State { baseDir = "", pendList = [], modList = [] }

addPending' :: FilePath -> PendList -> PendList
addPending' fp xs = maybe ((fp, True):xs) (const xs) (lookup fp xs)

togglePending' :: FilePath -> PendList -> PendList
togglePending' file xs = maybe xs (\tup -> (file, False):delete tup xs)
                                  (find ((==file) . fst) xs)

importSet :: GhostModule IntrinsicSet -> [FilePath]
importSet (GhostModule _ idecl _ _) = map makeRoot idecl
    where
      makeRoot :: ImportDecl -> FilePath
      makeRoot (ImportDecl decl) = addExtension (joinPath decl) "gl"
