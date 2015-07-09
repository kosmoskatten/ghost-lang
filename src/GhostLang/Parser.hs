module GhostLang.Parser
    ( parseGhostModule
    ) where

import GhostLang.Intrinsic (IntrinsicSet)
import GhostLang.Types (GhostModule)
import GhostLang.Parser.Grammar (ghostModuleDef)
import Text.Parsec (ParseError)
import Text.Parsec.String (parseFromFile)

-- | Parse a ghost module definition from file. Will throw exception
-- if file not is found.
parseGhostModule :: FilePath -> IO (Either ParseError (GhostModule IntrinsicSet))
parseGhostModule = parseFromFile ghostModuleDef
