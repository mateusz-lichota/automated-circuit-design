{-# LANGUAGE OverloadedStrings #-}

module CommonTypes where

import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Bits (Bits (shiftR, shift, testBit), (.&.), (.|.), xor, complement, zeroBits, shiftL, bit, popCount)
import Data.Maybe (fromJust)

import Data.Aeson
import Data.Text

type Cost = Int
type FuncId = Int



data Var = A | B | C1 | C0 deriving (Show, Eq)
allVars = [C1, C0, A, B]

instance Ord Var where
    compare A x = if x == A then EQ else LT
    compare B B = EQ
    compare B C0 = LT
    compare B C1 = LT
    compare C0 C1 = LT
    compare x y = if x == y then EQ else GT



data Gate = And | Nand | Or | Nor | Xor | Xnor deriving (Eq)
type GateEnv = Vector Gate

instance Show Gate where
    show And = "and"
    show Nand = "nand"
    show Or = "or"
    show Nor = "nor"
    show Xor = "xor"
    show Xnor = "xnor"


gateOp :: Gate -> (Int -> Int -> Int)
gateOp And = (.&.)
gateOp Nand = \x y -> complement (x .&. y)
gateOp Or = (.|.)
gateOp Nor = \x y -> complement (x .|. y)
gateOp Xor = xor
gateOp Xnor = \x y -> complement (x `xor` y)

instance Ord Gate where
    compare x y = EQ

allGates = V.fromList [And, Nand, Or, Nor, Xor, Xnor]


class GenvEvaluable a where
    eval :: a -> GateEnv -> FuncId
    permute :: a -> [(Var, Var)] -> a
    invert :: a -> [Var] -> a

instance GenvEvaluable Var where
    eval C1 _ = 0x00FF
    eval C0 _ = 0x0F0F
    eval A  _ = 0x3333
    eval B  _ = 0x5555

    permute x perms = fromJust $ lookup x perms
    invert  x invs  = undefined


instance ToJSON Var where
    toJSON A = String (pack "A")
    toJSON B = String  (pack "B")
    toJSON C0 = String (pack "C0")
    toJSON C1 = String (pack "C1")

instance FromJSON Var where
    parseJSON (String "A") = return A
    parseJSON (String "B") = return B
    parseJSON (String "C0") = return C0
    parseJSON (String "C1") = return C1
    parseJSON _ = fail "Expected A, B, C0 or C1"


instance ToJSON Gate where
    toJSON And = String (pack "And")
    toJSON Nand = String (pack "Nand")
    toJSON Or = String (pack "Or")
    toJSON Nor = String (pack "Nor")
    toJSON Xor = String (pack "Xor")
    toJSON Xnor = String (pack "Xnor")

instance FromJSON Gate where
    parseJSON (String "And") = return And
    parseJSON (String "Nand") = return Nand
    parseJSON (String "Or") = return Or
    parseJSON (String "Nor") = return Nor
    parseJSON (String "Xor") = return Xor
    parseJSON (String "Xnor") = return Xnor
    parseJSON _ = fail "Expected And, Nand, Or, Nor, Xor or Xnor"

