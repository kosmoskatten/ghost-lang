{-# LANGUAGE OverloadedStrings #-}
module GhostLang.LinkerTests
    ( -- Pre link semantic tests.
      checkEmptyModule
    , checkOneNonMainModule
    , checkMainModuleWithoutPatterns
    , checkOtherModuleWithPatterns
    , checkDuplicateMainModules
    , checkSingleCorrectMainModule
    , checkTwoCorrectModules

    -- Proc map tests.
    , findUndefinedProc
    , findDefinedProc
    , findDoubleDefinedProc

    -- Procedure resolving tests.
    , resolveLocalProc
    , resolveImportedProc
    , resolveProcInProc
    , resolveInLoopProc
    , resolveInConcProc
    , resolveUnimportedProc
    , resolveAmbiguousProc
    , resolveConflictingArityProc
    ) where

import Data.List (sortBy)
import GhostLang.Compiler.Linker ( ProcDef
                                 , semanticChecks
                                 , resolve
                                 , buildProcMap
                                 , findProcDefs
                                 )
import GhostLang.Interpreter (IntrinsicSet)
import GhostLang.Types ( ModuleSegment
                       , Value (..)
                       , GhostModule (..)
                       , ImportDecl (..)
                       , ModuleDecl (..)
                       , Pattern (..)
                       , Procedure (..)
                       , Operation (..)
                       )
import Test.HUnit
import Text.Parsec.Pos (initialPos)

-- | Test that the semantic checker is rejecting an empty
-- module list.
checkEmptyModule :: Assertion
checkEmptyModule = do
    let mods = [] :: [GhostModule IntrinsicSet]
    case semanticChecks mods of
      Right _  -> assertBool "Shall not accept empty list" False
      Left _   -> return ()

-- | Test that the semanic checker is rejecting a single module which
-- name not is main.
checkOneNonMainModule :: Assertion
checkOneNonMainModule = do
  let mods = [GhostModule (moduleDecl ["Other"]) [] [emptyPattern] []]
  case semanticChecks mods of
    Right _  -> assertBool "Shall not accept single non main module" False
    Left _   -> return ()

-- | Test that the semantic checker is rejecting a main module without
-- any patterns.
checkMainModuleWithoutPatterns :: Assertion
checkMainModuleWithoutPatterns = do
  let mods = [GhostModule (moduleDecl ["Main"]) [] [] []]
  case semanticChecks mods of
    Right _  -> assertBool "Shall not accept main module without patterns" False
    Left _   -> return ()

-- | Test that the semantic checker is rejecting an other module
-- having patterns.
checkOtherModuleWithPatterns :: Assertion
checkOtherModuleWithPatterns = do
  let mods = [ GhostModule (moduleDecl ["Main"]) [] [emptyPattern] []
             , GhostModule (moduleDecl ["Other"]) [] [emptyPattern] []
             ]
  case semanticChecks mods of
    Right _  -> assertBool "Shall not accept other module with patterns" False
    Left _   -> return ()

-- | Test that the semantic checker is rejecting duplicate main modules.
checkDuplicateMainModules :: Assertion
checkDuplicateMainModules = do
  let mods = [ GhostModule (moduleDecl ["Main"]) [] [emptyPattern] []
             , GhostModule (moduleDecl ["Main"]) [] [emptyPattern] []
             ]
  case semanticChecks mods of
    Right _  -> assertBool "Shall reject duplicate main modules" False
    Left _   -> return ()

-- | Test that the semantic checker is accepting a single main module
-- with one pattern.
checkSingleCorrectMainModule :: Assertion
checkSingleCorrectMainModule = do
  let mods = [GhostModule (moduleDecl ["Main"]) [] [emptyPattern] []]
  case semanticChecks mods of
    Right mods' -> mods @=? mods'
    _           -> assertBool "Shall accept" False

-- | Test that the semantic checker is accepting is accepting two
-- modules, one main module with a pattern and one other module
-- without patterns.
checkTwoCorrectModules :: Assertion
checkTwoCorrectModules = do
  let mods = [ GhostModule (moduleDecl ["Main"]) [] [emptyPattern] []
             , GhostModule (moduleDecl ["Other"]) [] [] []
             ]
  case semanticChecks mods of
    Right mods' -> mods @=? mods'
    _           -> assertBool "Shall accept" False

-- | Try to find an undefined procedure.
findUndefinedProc :: Assertion
findUndefinedProc = do
  let mods    = [] :: [GhostModule IntrinsicSet]
      procMap = buildProcMap mods
  [] @=? findProcDefs "foo" procMap

-- | Try to find a procedure defined once.
findDefinedProc :: Assertion
findDefinedProc = do
  let mods    = [ GhostModule (moduleDecl ["Main"]) [] [] [ emptyProcedure ] ]
      procMap = buildProcMap mods
  [("Main", emptyProcedure)] @=? findProcDefs "foo" procMap

-- | Find a procedure name defined in two modules.
findDoubleDefinedProc :: Assertion
findDoubleDefinedProc = do
  let mods    = [ GhostModule (moduleDecl ["Main"]) [] [] [ emptyProcedure ]
                , GhostModule (moduleDecl ["Other"]) [] [] [ emptyProcedure ]
                ]
      procMap = buildProcMap mods
      lhs     = sortBy procSort [ ("Main", emptyProcedure)
                                , ("Other", emptyProcedure) ]
      rhs     = sortBy procSort $ findProcDefs "foo" procMap
  lhs @=? rhs

-- | Resolve a module with a reference to a local procedure.
resolveLocalProc :: Assertion
resolveLocalProc = do
  let mods = [ GhostModule (moduleDecl ["Main"]) []
                           [ Pattern (initialPos "") "bar" 1
                                         [ Unresolved (initialPos "") "foo" [] ]
                           ] [ emptyProcedure ] ]
      -- The expected result with the unresolved reference resolved.
      res  = [ GhostModule (moduleDecl ["Main"]) []
                           [ Pattern (initialPos "") "bar" 1
                                         [ Call emptyProcedure [] ]
                           ] [ emptyProcedure ] ]
  case resolve mods of
    Right mods' -> res @=? mods'
    Left _      -> assertBool "Shall accept" False

-- | Resolve a module with a reference to an imported procedure.
resolveImportedProc :: Assertion
resolveImportedProc = do
  let mods = [ GhostModule (moduleDecl ["Main"]) 
                           [ ImportDecl ["Other", "Module"] ]
                           [ Pattern (initialPos "") "bar" 1
                                         [ Unresolved (initialPos "") "foo" [] ]
                           ] []
             , GhostModule (moduleDecl ["Other", "Module"]) [] []
                           [ emptyProcedure ]
             ]
      -- The expected result.
      res  = [ GhostModule (moduleDecl ["Main"]) 
                           [ ImportDecl ["Other", "Module"] ]
                           [ Pattern (initialPos "") "bar" 1
                                         [ Call emptyProcedure [] ]
                           ] []
             , GhostModule (moduleDecl ["Other", "Module"]) [] []
                           [ emptyProcedure ]
             ]

  case resolve mods of
    Right mods' -> res @=? mods'
    Left _      -> assertBool "Shall accept" False

-- | Resolve a procedure called from within a procedure.
resolveProcInProc :: Assertion
resolveProcInProc = do
  let mods = [ GhostModule (moduleDecl ["Main"]) []
               [ Pattern (initialPos "") "bar" 1
                 [ Unresolved (initialPos "") "caller" [] ]
               ]
               [ Procedure "caller" []
                 [ Unresolved (initialPos "") "foo" [] ]
               , emptyProcedure
               ]
             ]
      -- The resolved caller proc.
      caller = Procedure "caller" [] [ Call emptyProcedure [] ]
      -- The expected result.
      res  = [ GhostModule (moduleDecl ["Main"]) []
               [ Pattern (initialPos "") "bar" 1
                 [ Call caller []
                 ]
               ]
               [ caller, emptyProcedure 
               ]
             ]

  case resolve mods of
    Right mods' -> res @=? mods'
    Left _      -> assertBool "Shall accept" False

                           
-- | Resolve a module with a procedure reference inside a loop.
resolveInLoopProc :: Assertion
resolveInLoopProc = do
  let mods = [ GhostModule (moduleDecl ["Main"]) []
                           [ Pattern (initialPos "") "bar" 1
                                     [ Loop (Literal 1)
                                            [ Unresolved (initialPos "")
                                                         "foo" []
                                            ]
                                     ]
                           ] [ emptyProcedure ]
             ]
      -- The expected result.
      res  = [ GhostModule (moduleDecl ["Main"]) []
                           [ Pattern (initialPos "") "bar" 1
                                     [ Loop (Literal 1)
                                            [ Call emptyProcedure []
                                            ]
                                     ]
                           ] [ emptyProcedure]
             ]
  case resolve mods of
    Right mods' -> res @=? mods'
    Left _      -> assertBool "Shall accept" False

-- | Resolve a module with a procedure reference inside a concurrent section.
resolveInConcProc :: Assertion
resolveInConcProc = do
  let mods = [ GhostModule (moduleDecl ["Main"]) []
                           [ Pattern (initialPos "") "bar" 1
                                     [ Concurrently
                                            [ Unresolved (initialPos "")
                                                         "foo" []
                                            ]
                                     ]
                           ] [ emptyProcedure ]
             ]
      -- The expect result.
      res  = [ GhostModule (moduleDecl ["Main"]) []
                           [ Pattern (initialPos "") "bar" 1
                                     [ Concurrently
                                            [ Call emptyProcedure []
                                            ]
                                     ]
                           ] [ emptyProcedure]
             ]
  case resolve mods of
    Right mods' -> res @=? mods'
    Left _      -> assertBool "Shall accept" False

-- | Try to resolve a module with an unresolvable (unimported)
-- procedure.
resolveUnimportedProc :: Assertion
resolveUnimportedProc = do
  let mods = [ GhostModule (moduleDecl ["Main"]) 
                           []
                           [ Pattern (initialPos "") "bar" 1
                                         [ Unresolved (initialPos "") "foo" [] ]
                           ] []
             , GhostModule (moduleDecl ["Other", "Module"]) [] []
                           [ emptyProcedure ]
             ]

  case resolve mods of
    Right _  -> assertBool "Shall not accept unresolvable proc" False
    Left _   -> return ()

-- | Try to resolve an ambiguous procedure.
resolveAmbiguousProc :: Assertion
resolveAmbiguousProc = do
  let mods = [ GhostModule (moduleDecl ["Main"]) 
                           [ ImportDecl ["Other", "Module"]
                           , ImportDecl ["Other", "Module2"] ]
                           [ Pattern (initialPos "") "bar" 1
                                         [ Unresolved (initialPos "") "foo" [] ]
                           ] []
             , GhostModule (moduleDecl ["Other", "Module"]) [] []
                           [ emptyProcedure ]
             , GhostModule (moduleDecl ["Other", "Module2"]) [] []
                           [ emptyProcedure ]
             ]    

  case resolve mods of
    Right _  -> assertBool "Shall not accept ambiguous definitions" False
    Left _   -> return ()

-- | Try to resolve a procedure with different arity at definition and
-- use.
resolveConflictingArityProc :: Assertion
resolveConflictingArityProc = do
    let mods = [ GhostModule (moduleDecl ["Main"]) 
                           []
                           [ Pattern (initialPos "") "bar" 1
                                         [ Unresolved (initialPos "") "foo" 
                                                      [Literal 1] ]
                           ] [emptyProcedure]
               ]
    case resolve mods of
      Right _  -> assertBool "Shall not accept conflicting arity" False
      Left _   -> return ()

moduleDecl :: [ModuleSegment] -> ModuleDecl
moduleDecl = ModuleDecl (initialPos "")

emptyPattern :: Pattern IntrinsicSet
emptyPattern = Pattern (initialPos "") "" 0 []

emptyProcedure :: Procedure IntrinsicSet
emptyProcedure = Procedure "foo" [] []

procSort :: ProcDef a -> ProcDef a -> Ordering
procSort x y = fst x `compare` fst y
