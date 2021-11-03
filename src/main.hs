import Data.Maybe (fromJust, Maybe, fromMaybe, isJust)
import Data.List (intercalate, sort, nub, groupBy, permutations, group)
import Data.Bits (Bits (shiftR, shift, testBit), (.&.), (.|.), xor, complement, zeroBits, shiftL, bit)

import Control.DeepSeq

import qualified Data.Vector.Unboxed as V
import qualified Data.Vector as VB
import Data.Vector.Unboxed (Vector)

import Data.Vector.Algorithms.Intro as V.Algorithms.Intro (sort)

import Control.Parallel.Strategies (using, parListChunk, rdeepseq, rpar)

data Var = A | B | C1 | C0 deriving (Show, Eq)

data Gate = And | Nand | Or | Nor | Xor | Xnor | NotA | NotB | JustA | JustB | TT | FF deriving (Eq, Show)
type GateEnv = Vector GateNo
type GateNo = Int

allGates = [And, Nand, Or, Nor, Xor, Xnor, NotA, NotB, JustA, JustB, TT, FF]

gatesToUse :: Vector GateNo
gatesToUse = V.fromList [0, 1, 2, 3, 3, 5, 6, 8]

onlyNand = [Nand, JustA, JustB, TT, FF]

type Cost = Int
type FuncId = Int

cost :: Gate -> Cost
cost And = 6
cost Nand = 4
cost Or = 6
cost Nor = 4
cost Xor = 8
cost Xnor = 8
cost NotA = 3
cost NotB = 3
cost JustA = 0
cost JustB = 0
cost TT = 0
cost FF = 0


data Exp = BinApp Gate Exp Exp
         | Repl Int Exp Exp
         | Not  Exp 
         | Var  Var 
         | T
         | F
        deriving (Eq)
type VarEnv = [(Var, Int)]

instance Show Exp where
    show (BinApp And  e1 e2) = show e1 ++ " * " ++ show e2
    show (BinApp Nand e1 e2) = show (Not (BinApp And e1 e2))
    show (BinApp Or   e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")"
    show (BinApp Nor  e1 e2) = show (Not (BinApp Or e1 e2))
    show (BinApp Xor  e1 e2) = show e1 ++ " ⊕ " ++ show e2
    show (BinApp Xnor e1 e2) = show (Not (BinApp Xor e1 e2))
    show (BinApp JustA e1 e2) = show e1
    show (BinApp JustB e1 e2) = show e2
    show (BinApp NotA e1 e2) = show (Not e1)
    show (BinApp NotB e1 e2) = show (Not e2)
    show (BinApp TT e1 e2) = "1"
    show (BinApp FF e1 e2) = "0"
    show (Var v)      = show v
    show (Not (Var v))= show v ++ "'"
    show (Not (BinApp Or e1 e2))= show (BinApp Or e1 e2) ++ "'"
    show (Not e)      = "(" ++ show e ++ ")'"
    show (Repl x e1 e2) = "(" ++ show e1 ++ " G" ++ show x ++ " " ++ show e2 ++ ")"
    show T = "1"
    show F = "0"

data B4digit = Q0 | Q1 | Q2 | Q3
instance Show B4digit where
    show Q0 = "0"
    show Q1 = "1"
    show Q2 = "2"
    show Q3 = "3"

type B4num = (B4digit, B4digit, B4digit, B4digit)

spec :: B4num -> Exp 
spec (d3, d2, d1, d0) = Prelude.foldl1 (BinApp Or) [
     BinApp And (BinApp And (Not (Var C1)) (Not (Var C0))) (spec00 d3)
    ,BinApp And (BinApp And (Not (Var C1)) (Var C0))       (spec01 d2)
    ,BinApp And (BinApp And (Var C1)       (Not (Var C0))) (spec10 d1)
    ,BinApp And (BinApp And (Var C1)       (Var C0))       (spec11 d0)
    ]

spec00, spec01, spec10, spec11 :: B4digit -> Exp
spec00 Q0 = BinApp Xor (Var A) (Var B)
spec00 Q1 = BinApp Xnor (Var A) (Var B)
spec00 Q2 = T
spec00 Q3 = F

spec01 Q0 = Var A
spec01 Q1 = Not (Var A)
spec01 Q2 = Not (Var B)
spec01 Q3 = Var B

spec10 Q0 = BinApp And (Var A)       (Var B)
spec10 Q1 = BinApp And (Var A)       (Not (Var B))
spec10 Q2 = BinApp And (Not (Var A)) (Not (Var B))
spec10 Q3 = BinApp And (Not (Var A)) (Var B)

spec11 Q0 = BinApp Or (Var A)       (Var B)
spec11 Q1 = BinApp Or (Not (Var A)) (Var B)
spec11 Q2 = BinApp Or (Var A)       (Not (Var B))
spec11 Q3 = BinApp Or (Not (Var A)) (Not (Var B))


specTable :: B4num -> [[String]]
specTable (d3, d2, d1, d0) = 
    [
        [     "Digits "     ,"C_1","C_2",     "Output"    ],
        ["D_3 = " ++ show d3," 0 "," 0 ", show (spec00 d3)],
        ["D_2 = " ++ show d2," 0 "," 1 ", show (spec01 d2)],
        ["D_1 = " ++ show d1," 1 "," 0 ", show (spec10 d1)],
        ["D_0 = " ++ show d0," 1 "," 1 ", show (spec11 d0)]
    ]

ppTable :: [[String]] -> String
ppTable t = unlines $ Prelude.map (intercalate " | ") t

eval :: VarEnv -> Exp -> Int
eval = e
    where
        e env (BinApp And x y)   = e env x .&. e env y
        e env (BinApp Nand x y)  = complement $ e env x .&. e env y
        e env (BinApp Or x y)    = e env x .|. e env y
        e env (BinApp Nor x y)   = complement $ e env x .|. e env y
        e env (BinApp Xor x y)   = e env x `xor` e env y
        e env (BinApp Xnor x y)  = complement $ e env x `xor` e env y
        e env (BinApp JustA x y) = e env y
        e env (BinApp JustB x y) = e env x
        e env (BinApp NotA x y)  = complement $ e env x
        e env (BinApp NotB x y)  = complement $ e env y
        e env (BinApp TT _ _)    = complement zeroBits
        e env (BinApp FF _ _)    = zeroBits
        e env (Not x)            = complement $ e env x
        e env (Var v)            = fromJust $ lookup v env
        e env T                  = complement zeroBits 
        e env F                  = zeroBits
        e env Repl {}            = error "evaluated expression with replacements"


repl :: Exp -> GateEnv -> Exp
repl exp env = r exp
    where
        r (BinApp g e1 e2) = BinApp g (r e1) (r e2)
        r (Not e) = Not (r e)
        r (Repl x e1 e2) = BinApp (allGates !! (env V.! x)) (r e1) (r e2)
        r x = x

allCombs :: VarEnv
allCombs = [(A, 0x5555), (B, 0x3333), (C0, 0x0F0F), (C1, 0x00FF)]



applyFunc3 :: FuncId -> (Int, Int, Int) -> Int
applyFunc3 f (a,b,c) = Prelude.foldl1 (.|.) [if testBit f (7 - lvl i) then bit i else 0 | i <- [0..7]]
    where lvl i = shift (a .&. bit i) (2-i) .|. shift (b .&. bit i) (1-i) .|. shift (c .&. bit i) (-i)

applyFunc4 :: FuncId -> (Int, Int, Int, Int) -> Int
applyFunc4 f (a,b,c,d) = Prelude.foldl1 (.|.) [if testBit f (15 - lvl i) then bit i else 0 | i <- [0..15]]
    where lvl i = 
           shift (a .&. bit i) (3-i) .|. 
           shift (b .&. bit i) (2-i) .|. 
           shift (c .&. bit i) (1-i) .|.
           shift (d .&. bit i) (0-i)


smallestP3 :: FuncId -> FuncId
smallestP3 f = Prelude.minimum [applyFunc3 f (p!!0, p!!1, p!!2) | p <- permutations vars]
    where vars = [0x55, 0x33, 0x0F]

smallestP4 :: FuncId -> FuncId
smallestP4 f = Prelude.minimum [applyFunc4 f (p!!0, p!!1, p!!2, p!!3) | p <- permutations vars]
    where vars = Prelude.map snd allCombs

--- generate all possible n-element arrays of GateNo's
gateCombos :: Vector GateNo -> Int -> VB.Vector GateEnv
gateCombos gs 1 = VB.map V.singleton (VB.convert gs)
gateCombos gs n = VB.concatMap (\group -> VB.map (`V.cons` group) (VB.convert gs)) gcs
    where gcs = gateCombos gs (n-1)




scheme3, scheme4_6, scheme4_10 :: (Int, Int, Exp)
scheme3 = (6, 3, Repl 1 
    (Repl 2 
        (Repl 4 (Var A) (Var B)) 
        (Repl 5 (Var A) (Var C0))
    ) 
    (Repl 3 
        (Repl 5 (Var A) (Var C0)) 
        (Repl 0 (Var B) (Var C0))
    ))

scheme4_6 = (6, 4, Repl 0
    (Repl 5
        (Repl 3 (Var A) (Var B))
        (Repl 2 (Var B) (Var C0))
    )
    (Repl 4
        (Repl 2 (Var B) (Var C0))
        (Repl 1 (Var C0) (Var C1))
    )
    )

scheme4_10 = (10, 4, Repl 10
    (Repl 9
        (Repl 8 
            r7
            r5
        )
        r6
    )
    (Repl 4
        r6
        (Repl 3
            r1 
            r7
        )
    )
    )
    where r5 = Repl 5 (Var B) (Var C0)
          r2 = Repl 2 (Var C0) (Var C1)
          r1 = Repl 1 (Var C1) (Var A)
          r7 = Repl 7 (Var A) (Var B)
          r6 = Repl 6 r5 r2

allResults :: (Int, Int, Exp) -> VarEnv -> Vector (FuncId, Cost)
allResults (n, numVars, exp) combs = V.modify V.Algorithms.Intro.sort ar
    where 
        ar = V.filter (/=(-1, -1)) $ V.imap imapHelper table

        imapHelper :: Int -> Cost -> (FuncId, Cost)
        imapHelper i cst = if cst < 9999 then (i, cst) else (-1, -1)
        

        table :: Vector Cost
        table = minCosts $ V.convert $ VB.map compOne gcs
        
        gcs :: VB.Vector GateEnv
        gcs = gateCombos gatesToUse n

        compOne :: GateEnv -> (FuncId, Cost)
        compOne gc = (canonicForm (evalRes gc), gcCost gc)

        canonicForm = (V.!) eqClass4 

        evalRes gc = mask $ eval combs (repl exp gc)
        mask x = x .&. complement (shiftL (complement zeroBits) (2^numVars))

        gcCost gc = V.sum $ V.map (cost.(allGates!!)) gc

        minCosts :: Vector (FuncId, Cost) -> Vector Cost
        minCosts = V.foldl func (V.replicate (2^(2^4)) 9999)
            where 
                func :: Vector Cost -> (FuncId, Cost) -> Vector Cost
                func vect (fid, cst) = if cst < vect V.! fid then vect V.// [(fid, cst)] else vect 

-- eqClassN is a lookup table to efficiently find the smallest representative
-- of the permutation invariant euqivalence class

eqClass3 :: Vector FuncId
eqClass3 = V.map smallestP3 (V.enumFromN 0 (2^8))

eqClass4 :: Vector FuncId
eqClass4 = V.map smallestP4 (V.enumFromN 0 (2^16))

eqClasses4 :: Vector FuncId
eqClasses4 = V.uniq $ V.modify V.Algorithms.Intro.sort eqClass4

s4_6res, s4_10res :: Vector (FuncId, Cost)
s4_6res = allResults scheme4_6 allCombs
s4_10res = allResults scheme4_10 allCombs

main :: IO ()
-- main = print $ length $ allFuncs (allResults scheme4_1 allCombs) 4
main = print $ V.length s4_6res
-- main = print $ length $ Prelude.map head $ group $ toList $ V.modify V.Algorithms.Intro.sort eqClass4
