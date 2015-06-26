{-# LANGUAGE GADTs #-}
module GhostLang.Types
    ( Label
    , Weight
    , InstructionSet (..)
    , Pattern (..)
    , Procedure (..)
    , Operation (..)
    ) where

import Data.Text (Text, unpack)
import GhostLang.InterpreterM (InterpreterM)
import Text.Printf (printf)

-- | Instruction set type class.
class InstructionSet a where
    exec :: a -> InterpreterM ()

type Label = Text
type Weight = Int

data Pattern a where
    Pattern :: InstructionSet a => 
               !Label -> !Weight -> ![Operation a] -> Pattern a

data Procedure a where
    Procedure :: InstructionSet a => !Label -> ![Operation a] -> Procedure a

data Operation a where
    Invoke :: InstructionSet a => !a -> Operation a
    -- ^ Invoke is the operation of invoking a simple element of the
    -- ghost language. E.g. invoking a http get.

    Call :: InstructionSet a => !(Procedure a) -> Operation a
    -- ^ Call is the operation of calling a procedure. The procedure
    -- is given its arguments in a local context of the Interpreter
    -- monad.

    Unresolved :: InstructionSet a => !Label -> Operation a

-- | Show instance for Pattern.
instance Show a => Show (Pattern a) where
    show (Pattern label weight ops) = 
        printf "Pattern %s %d %s\n" (unpack label) weight (show ops)

-- | Show instance for Procedure.
instance Show a => Show (Procedure a) where
    show (Procedure label ops) = printf "Procedure %s %s\n" (unpack label) 
                                                            (show ops)

-- | Show instance for Operation.
instance Show a => Show (Operation a) where
    show (Invoke instr)     = printf "Invoke %s\n" (show instr)
    show (Call proc)        = printf "Call %s\n" (show proc)
    show (Unresolved label) = printf "Unresolved %s\n" (unpack label)
