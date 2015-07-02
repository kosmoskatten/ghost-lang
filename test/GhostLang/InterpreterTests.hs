module GhostLang.InterpreterTests
    ( simpleSequencePattern
    , manySimpleSequencePatterns
    , nonNestedLoopPattern
    ) where

import Control.Concurrent.STM (newTVarIO, readTVarIO)
import Control.Monad (forM)
import Data.List (foldl')
import Data.Text (Text)
import GHC.Int (Int64)
import GhostLang.Counter ( Counter (..)
                         , emptyCounter
                         , getPatternRuns
                         , getTotalPatternRuns
                         , getTotalProcCalls
                         )
import GhostLang.Generators ( SimpleSequencePattern (..)
                            , ManySimpleSequencePatterns (..)
                            , NonNestedLoopPattern (..)
                            )
import GhostLang.Interpreter (execPattern)
import GhostLang.InterpreterM (runInterpreter)
import GhostLang.Types (Value (..), Pattern (..), Operation (..))
import Test.QuickCheck
import Test.QuickCheck.Monadic

-- | Property to test a simple sequence pattern of
-- instructions. There's a fresh counter instance at each invokation.
simpleSequencePattern :: SimpleSequencePattern -> Property
simpleSequencePattern (SimpleSequencePattern p) =
    monadicIO $ do
      inpC    <- run (newTVarIO emptyCounter)
      run (runInterpreter [inpC] $ execPattern p)
      counter <- run (readTVarIO inpC)

      -- As many instructions invoked as the number of operations in
      -- the list.
      assert $ (opsLength p) == instrInvoked counter

      -- There shall be no loop runs.
      assert $ 0 == loopRuns counter

      -- This pattern shall have been run once.
      assert $ 1 == getPatternRuns (patternName p) counter

      -- The total number of patterns is one as well.
      assert $ 1 == getTotalPatternRuns counter

      -- No procedures called.
      assert $ 0 == getTotalProcCalls counter

-- | Property to test a list of simple sequence patterns. The property
-- is about to test the handling of multiple counters (e.g. a global
-- counter and local counters) for pattern execution.
manySimpleSequencePatterns :: ManySimpleSequencePatterns -> Property
manySimpleSequencePatterns (ManySimpleSequencePatterns ps) =
    monadicIO $ do
      globalC <- run (newTVarIO emptyCounter)
      locals  <- run (forM ps $ \p -> do
                        localC <- newTVarIO emptyCounter
                        runInterpreter [globalC, localC] $ execPattern p
                        return =<< readTVarIO localC
                     )
      global <- run (readTVarIO globalC)

      -- As many instructions invoked globaly as the sum of
      -- operations in the patterns.
      let totalOps = sum $ map opsLength ps
      assert $ totalOps == instrInvoked global

      -- Also, the sum of the local counters shall be the same as well.
      assert $ totalOps == (sum $ map instrInvoked locals)

      -- The total number of pattern runs for the global counter shall
      -- be equal to the number of patterns supplied to the property.
      let totalPatterns = fromIntegral $ length ps
      assert $ totalPatterns == getTotalPatternRuns global

      -- Also, the sum of the local counters shall be the same as well.
      assert $ totalPatterns == (sum $ map getTotalPatternRuns locals)

-- | Property to the test the expansion and execution of loops.
nonNestedLoopPattern :: NonNestedLoopPattern -> Property
nonNestedLoopPattern (NonNestedLoopPattern p) =
    monadicIO $ do
      inpC    <- run (newTVarIO emptyCounter)
      run (runInterpreter [inpC] $ execPattern p)
      counter <- run (readTVarIO inpC)

      -- Count the number of invoke operations and loop
      -- operations.
      let (invokes, loops) = countInvokesAndNonNestedLoops p

      -- As many instructions invoked as invoke operations times the
      -- loop expansions.
      assert $ invokes == instrInvoked counter

      -- As many loop instructions as found in the pattern.
      assert $ loops == loopRuns counter

opsLength :: Pattern a -> Int64
opsLength (Pattern _ _ ops) = fromIntegral $ length ops

patternName :: Pattern a -> Text
patternName (Pattern name _ _) = name

-- | Count the number of invoke instructions (incl. expanding the
-- loops) and the number of loop commands.
countInvokesAndNonNestedLoops :: Pattern a -> (Int64, Int64)
countInvokesAndNonNestedLoops (Pattern _ _ ops) = foldl' count (0, 0) ops
    where
      count :: (Int64, Int64) -> Operation a -> (Int64, Int64)
      count (x, y) (Invoke _)       = (x + 1, y)
      count (x,y) (Loop times ops') =
          let Const times' = times
          in (x + fromIntegral (length ops') * times', y + 1)
      count x _                     = x
