module Exps where

import qualified Data.Vector as V
import Data.Vector (Vector)

import Data.Bits ((.&.), complement)

import CommonTypes ( Gate(..), Var(..), GenvEvaluable (..), gateOp )
import Data.Maybe (fromJust)

data Exp = BinApp Gate Exp Exp
         | Repl Int Exp Exp
         | Not  Exp
         | Var  Var
         | T
         | F
        deriving (Eq)
type VarEnv = [(Var, Int)]

instance Ord Exp where
    compare x y = EQ

instance Show Exp where
    show (BinApp And  e1 e2) = "(" ++ show e1 ++ " and " ++ show e2 ++ ")"
    show (BinApp Nand e1 e2) = "(" ++ show e1 ++ " nand " ++ show e2 ++ ")"
    show (BinApp Or   e1 e2) = "(" ++ show e1 ++ " or " ++ show e2 ++ ")"
    show (BinApp Nor  e1 e2) = "(" ++ show e1 ++ " nor " ++ show e2 ++ ")"
    show (BinApp Xor  e1 e2) = "(" ++ show e1 ++ " xor " ++ show e2 ++ ")"
    show (BinApp Xnor e1 e2) = "(" ++ show e1 ++ " xnor " ++ show e2 ++ ")"
    show (Var v)      = show v
    show (Not v)      = show v ++ "'"
    show (Repl x e1 e2) = "(" ++ show e1 ++ " G" ++ show x ++ " " ++ show e2 ++ ")"
    show T = "1"
    show F = "0"


instance GenvEvaluable Exp where
    eval exp gEnv = 0xFFFF .&. e exp where
        e (BinApp g x y)     = gateOp g (e y) (e x)
        e (Not x)            = complement $ e x
        e (Var v)            = eval v gEnv 
        e T                  = 0xFFFF
        e F                  = 0x0000
        e (Repl x exp1 exp2) = e (BinApp (gEnv V.! x) exp1 exp2)

    permute exp perms = r exp
        where
            r (BinApp g e1 e2) = BinApp g (r e1) (r e2)
            r (Not e) = Not (r e)
            r (Var v) = Var (permute v perms)
            r x = x

    invert exp invs = r exp
        where
            r (BinApp g e1 e2) = BinApp g (r e1) (r e2)
            r (Repl x e1 e2) = Repl x (r e1) (r e2)
            r (Not e) = Not (r e)
            r (Var x) = if x `elem` invs then Not (Var x) else Var x
            r x = x
