module GhostLang.Scope
    ( Scope
    , emptyScope
    , fromList
    , lookup
    ) where

import Data.Text (Text)
import GhostLang.Types (Value)
import Prelude hiding (lookup)
import qualified Data.Map.Strict as Map

newtype Scope = Scope (Map.Map Text Value)
    deriving Show

-- | Make an empty scope.
emptyScope :: Scope
emptyScope = Scope Map.empty

-- | Make a scope from the provided list.
fromList :: [(Text, Value)] -> Scope
fromList = Scope . Map.fromList

-- | Lookup a variable from the scope.
lookup :: Text -> Scope -> Maybe Value
lookup k (Scope m) = Map.lookup k m
