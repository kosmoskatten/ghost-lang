{-# LANGUAGE OverloadedStrings #-}
module GhostLang.IntrinsicTests 
    ( delayCommand
    ) where

import Control.Monad (when)
import Data.Time (NominalDiffTime, diffUTCTime, getCurrentTime)
import GhostLang.Intrinsic (IntrinsicSet (..))
import GhostLang.Interpreter (execPattern)
import GhostLang.InterpreterM (runInterpreter)
import GhostLang.Types ( Value (..)
                       , TimeUnit (..)
                       , Pattern (..)
                       , Operation (..)
                       )
import Test.HUnit
import Text.Printf (printf)

-- | Test case to verify that the Delay command is delaying the
-- intended duration of time.
delayCommand :: Assertion
delayCommand = do
  let p = Pattern "pa1" 1
          [ Invoke $ Delay $ MSec (Literal 500)
          ]
  -- Time the duration for the pattern execution.
  dur <- timedAction $ runInterpreter [] $ execPattern p

  -- Let the range be generous due to the conditions for testing.
  let minDur = realToFrac (0.5 :: Double)
      maxDur = realToFrac (0.9 :: Double)
  dur `assertGTE` minDur
  dur `assertLTE` maxDur

assertGTE :: (Ord a, Show a) => a -> a -> Assertion
assertGTE x y =
    when (x < y) $ 
         assertString (printf "%s shall be greater than or equal to %s" 
                              (show x) (show y))

assertLTE :: (Ord a, Show a) => a -> a -> Assertion
assertLTE x y =
    when (x > y) $
         assertString (printf "%s shall be lesser than or equal to %s"
                              (show x) (show y))
        

timedAction :: IO () -> IO NominalDiffTime
timedAction act = do
  t1 <- getCurrentTime
  act
  t2 <- getCurrentTime
  return $ t2 `diffUTCTime` t1
