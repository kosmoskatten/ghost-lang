module GhostLang.Compiler
    ( compileAndLink
    , fromFiles
    ) where

import GhostLang.Compiler.Linker (linkProgram)
import GhostLang.Compiler.ModuleFileReader ( runModuleReader
                                           , parseGhostModules )
import GhostLang.Interpreter (IntrinsicSet)
import GhostLang.Types (GhostModule, Program)

-- | Compile and link a set of modules from file. The file pointed out is the
-- module implementing "Main". From its dependencies the reading is
-- continued. If any fail occurs the result will be an error message,
-- otherwise the set of parsed modules.
compileAndLink :: FilePath -> IO (Either String (Program IntrinsicSet))
compileAndLink root = do
  mods <- fromFiles root
  let maybeProgram = mods >>= linkProgram
  return maybeProgram

-- | Read a set of modules from files. The file pointed out is the
-- module implementing "Main". From its dependencies the reading is
-- continued. If any fail occurs the result will be an error message,
-- otherwise the set of parsed modules.
fromFiles :: FilePath -> IO (Either String [GhostModule IntrinsicSet])
fromFiles = runModuleReader . parseGhostModules
