module GhostLang.InterpreterMProps
    ( evalValueP
    , evalTimeUnitP
    ) where

import GHC.Int (Int64)
import GhostLang.CommonGenerators ()
import GhostLang.Interpreter.InterpreterM ( runInterpreterTest
                                          , evalValue
                                          , evalTimeUnit )
import GhostLang.Types (Value (..), TimeUnit (..))
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- | Property to test the evaluation of values.
evalValueP :: Value -> Property
evalValueP v@(Literal x) = 
    monadicIO $ do
      x' <- run $ runInterpreterTest (evalValue v)
      assert $ x == x'

-- Just approve the other cases for now.
evalValueP _ = monadicIO $ return ()

-- | Property to test the evaluation of time units.
evalTimeUnitP :: TimeUnit -> Property
evalTimeUnitP t@(USec v) = evalTimeUnitP'       1 t v
evalTimeUnitP t@(MSec v) = evalTimeUnitP'    1000 t v
evalTimeUnitP t@(Sec  v) = evalTimeUnitP' 1000000 t v

evalTimeUnitP' :: Int64 -> TimeUnit -> Value -> Property
evalTimeUnitP' factor t (Literal x) =
    monadicIO $ do
      x' <- run $ runInterpreterTest (evalTimeUnit t)
      assert $ (x * factor) == fromIntegral x'

-- Just approve the other cases for ever :-)
evalTimeUnitP' _ _ _ = monadicIO $ return ()


