{-# LANGUAGE ScopedTypeVariables #-}
{-# language BangPatterns #-}

{-# LANGUAGE ScopedTypeVariables #-}
{-# language BangPatterns #-}
import Data.Maybe (fromJust, Maybe, fromMaybe, isJust, catMaybes)
import Data.List (intercalate, sort, nub, groupBy, permutations, group)
import Data.Bits (Bits (shiftR, shift, testBit), (.&.), (.|.), xor, complement, zeroBits, shiftL, bit, popCount)
import qualified Data.Vector as V
import Data.Vector (Vector)

import Numeric (showHex)


import qualified Data.Vector.Algorithms.Intro as V (sort, sortBy)

import Dags (genDags, Dag, DagNode (N, Inp))


data Var = A | B | C1 | C0 deriving (Show, Eq)
allVars = [C1, C0, A, B]

instance Ord Var where
    compare A x = if x == A then EQ else LT
    compare B B = EQ
    compare B C0 = LT
    compare B C1 = LT
    compare C0 C1 = LT
    compare x y = if x == y then EQ else GT



data Gate = And | Nand | Or | Nor | Xor | Xnor deriving (Eq, Show)
type GateEnv = Vector Gate

allGates = V.fromList [And, Nand, Or, Nor, Xor, Xnor]



type Cost = Int
type FuncId = Int

cost :: Gate -> Cost
cost And = 6
cost Nand = 4
cost Or = 6
cost Nor = 4
cost Xor = 8
cost Xnor = 8


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

data B4digit = Q0 | Q1 | Q2 | Q3
instance Show B4digit where
    show Q0 = "0"
    show Q1 = "1"
    show Q2 = "2"
    show Q3 = "3"

digits :: [B4digit]
digits = [Q0, Q1, Q2, Q3]



type B4num = (B4digit, B4digit, B4digit, B4digit)

allb4nums :: [B4num]
allb4nums = [(a,b,c,d) | a <- digits, b <- digits, c <- digits, d<-digits]


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

eval :: Exp -> Int
eval exp = 0xFFFF .&. e  exp
    where
        e (BinApp And x y)   = e x .&. e y
        e (BinApp Nand x y)  = complement $ e x .&. e y
        e (BinApp Or x y)    = e x .|. e y
        e (BinApp Nor x y)   = complement $ e x .|. e y
        e (BinApp Xor x y)   = e x `xor` e y
        e (BinApp Xnor x y)  = complement $ e x `xor` e y
        e (BinApp JustA x y) = e y
        e (BinApp JustB x y) = e x
        e (BinApp NotA x y)  = complement $ e x
        e (BinApp NotB x y)  = complement $ e y
        e (BinApp TT _ _)    = complement zeroBits
        e (BinApp FF _ _)    = zeroBits
        e (Not x)            = complement $ e x
        e (Var C1)           = 0x00FF
        e (Var C0)           = 0x0F0F
        e (Var A)            = 0x3333
        e (Var B)            = 0x5555
        e T                  = complement zeroBits
        e F                  = zeroBits
        e Repl {}            = error "evaluated expression with replacements"

allCombs :: VarEnv
allCombs = map (\v -> (v, eval (Var v))) allVars


applyFunc :: FuncId -> [Int] -> Int
applyFunc f [a,b,c,d] = 0xFFFF .&. Prelude.foldl1 (.|.) [if testBit f (15 - lvl i) then bit i else 0 | i <- [0..15]]
    where lvl i =
           shift (a .&. bit i) (3-i) .|.
           shift (b .&. bit i) (2-i) .|.
           shift (c .&. bit i) (1-i) .|.
           shift (d .&. bit i) (0-i)
applyFunc f _ = error "error"


--- returns the lexicographically smallest permutation-equivalent function
smallestP :: FuncId -> FuncId
smallestP f = Prelude.minimum [applyFunc f p | p <- permutations vars]
    where vars = Prelude.map snd allCombs



--- generate all possible n-element arrays of gates
gateCombos :: GateEnv -> Int -> Vector GateEnv
gateCombos gs 1 = V.map V.singleton gs
gateCombos gs n = V.concatMap (\group -> V.map (`V.cons` group) gs) gcs
        where gcs = gateCombos gs (n-1)



scheme4_6 = (6, Repl 0
    (Repl 5
        (Repl 3 (Var A) (Var B))
        (Repl 2 (Var B) (Var C0))
    )
    (Repl 4
        (Repl 2 (Var B) (Var C0))
        (Repl 1 (Var C0) (Var C1))
    )
    )

scheme4_7 = (7, Repl 0
    (Repl 6
        (Repl 4 (Var A) (Var B))
        (Repl 3 (Var B) (Var C0))
    )
    (Repl 5
        (Repl 2 (Var C0) (Var C1))
        (Repl 1 (Var C1) (Var A))
    )
    )

scheme4_8_1 = (8, Repl 0
    (Repl 6
        (Repl 4 (Var A) (Var B))
        r3
    )
    (Repl 7
        (Repl 5
            (Repl 2 (Var C0) (Var C1))
            (Repl 1 (Var C1) (Var A))
        )
        r3)
    )
    where r3 = Repl 3 (Var B) (Var C0)

scheme4_8_2 = (8,
    Repl 0
        (Repl 7
            (Repl 6
                (Repl 5 r1 r2)
                r3
            )
            r4
        )
        r2
    )
    where r1 = Repl 1 (Var A)  (Var B)
          r2 = Repl 2 (Var B)  (Var C0)
          r3 = Repl 3 (Var C0) (Var C1)
          r4 = Repl 4 (Var C1) (Var A)

scheme4_8_3 = (8,
    Repl 0
        (Repl 7
            (Repl 6
                (Repl 5 r1 r2)
                (Var B)
            )
            r4
        )
        r3
    )
    where r1 = Repl 1 (Var A)  (Var B)
          r2 = Repl 2 (Var B)  (Var C0)
          r3 = Repl 3 (Var C0) (Var C1)
          r4 = Repl 4 (Var C1) (Var A)

scheme4_8_4 = (8,
    Repl 0
        (Repl 7
            (Repl 6
                (Repl 5 r1 r2)
                (Var B)
            )
            r4
        )
        r3
    )
    where r1 = Repl 1 (Var A)  (Var B)
          r2 = Repl 2 (Var B)  (Var C0)
          r3 = Repl 3 (Var C0) (Var C1)
          r4 = Repl 4 (Var C1) (Var A)

scheme4_9_1 = (9,
    Repl 0
        (Repl 7
            (Repl 5
                r1
                r2
            )
            r3
        )
        (Repl 8
            r2
            (Repl 6
                r3
                r4
            )
        )
    )
    where r1 = Repl 1 (Var A)  (Var B)
          r2 = Repl 2 (Var B)  (Var C0)
          r3 = Repl 3 (Var C0) (Var C1)
          r4 = Repl 4 (Var C1) (Var A)

scheme4_10 = (10, Repl 10
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



allResults :: (Int, Exp) -> Vector (FuncId, Exp, Cost)
allResults (numGates, exp) = V.modify V.sort ar
    where
        ar :: Vector (FuncId, Exp, Cost)
        ar = V.filter (/=(-1, F, -1)) $ V.imap imapHelper tableForm

        imapHelper :: Int -> (Exp, Cost) -> (FuncId, Exp, Cost)
        imapHelper i (exp, cst) = if cst < 9999 then (i, exp, cst) else (-1, F, -1)

        -- 2^(2^4) size vector with (Exp, Cost) /= (F, 9999) at position fid
        -- if function fid was constructed
        tableForm :: V.Vector (Exp, Cost)
        tableForm = minCosts $ V.convert $ V.map compOne (gateCombos allGates numGates)

        compOne :: GateEnv -> (FuncId, Exp, Cost)
        compOne gc = (fid, exp', gcCost gc)
            where exp' = fillGates exp gc
                  fid  = canonize $ mask $ eval exp'
                  mask x = x .&. complement (shiftL (complement zeroBits) (2^4))
                  gcCost gc = V.sum $ V.map cost gc

        fillGates :: Exp -> GateEnv -> Exp
        fillGates exp env = r exp
            where
                r (BinApp g e1 e2) = BinApp g (r e1) (r e2)
                r (Not e) = Not (r e)
                r (Repl x e1 e2) = BinApp (env V.! x) (r e1) (r e2)
                r x = x

        minCosts :: Vector (FuncId, Exp, Cost) -> Vector (Exp, Cost)
        minCosts = V.foldl func (V.replicate (2^(2^4)) (F, 9999))
            where
                func :: Vector (Exp, Cost) -> (FuncId, Exp, Cost) -> Vector (Exp, Cost)
                func vect (fid, exp, cst) = if cst < snd (vect V.! fid) then vect V.// [(fid, (exp, cst))] else vect

canonize :: FuncId -> FuncId
canonize f = eqClass V.! f



augmentWithInputInversions :: Vector (FuncId, Exp, Cost) -> Vector (FuncId, Exp, Cost)
augmentWithInputInversions rs = combineResults [V.concatMap (\res -> V.map (invertRes res) allVarLists) rs]

invertRes :: (FuncId, Exp, Cost) -> [Var] -> (FuncId, Exp, Cost)
invertRes (f, exp, c) invs = (canonize $ eval exp', exp', c + (3 * length invs))
    where exp' = invertExp exp invs


-- returns an input combo and a list of vars that were inverted
allVarLists :: V.Vector [Var]
allVarLists = V.fromList $ map gen [0..15]
    where
        gen :: Int -> [Var]
        gen x = [allVars!!i | i <- [0..3], testBit x i]


invertExp :: Exp -> [Var] -> Exp
invertExp exp env = r exp
    where
        r (BinApp g e1 e2) = BinApp g (r e1) (r e2)
        r (Not e) = Not (r e)
        r (Var x) = if x `elem` env then Not (Var x) else Var x
        r x = x

noInvertExpIsOriginal :: Bool
noInvertExpIsOriginal = and [eval exp == eval (invertExp exp []) | exp <- map extractExp (V.toList s7res)]
    where extractExp = \(_, e, _) -> e

twiceInvertExpIsOriginal :: Bool
twiceInvertExpIsOriginal = and [eval exp == eval (invertExp (invertExp exp invs) invs) | exp <- map extractExp (V.toList s7res), invs <- V.toList allVarLists]
    where extractExp = \(_, e, _) -> e


-- eqClassN is a lookup table to efficiently find the smallest representative
-- of the permutation invariant euqivalence class

eqClass :: Vector FuncId
eqClass = V.map smallestP (V.enumFromN 0 (2^16))

testEqClass :: Bool
testEqClass = V.all (\x -> eqClass V.! (eqClass V.! x) == eqClass V.! x) eqClass

eqClasses :: Vector FuncId
eqClasses = V.uniq $ V.modify V.sort eqClass

s6res, s7res, s8_1res, s8_2res, s8_3res :: Vector (FuncId, Exp, Cost)
s6res = allResults scheme4_6
s7res = allResults scheme4_7

s8_1res = allResults scheme4_8_1
s8_2res = allResults scheme4_8_2
s8_3res = allResults scheme4_8_3

--union of s8_1 and s8_2 is 3742
--intersection is therefore 3124



combineResults :: [Vector (FuncId, Exp, Cost)] -> Vector (FuncId, Exp, Cost)
combineResults rs = V.fromList $ remDup sorted
    where
        sorted = V.toList $ V.modify V.sort concatenated
        concatenated = foldl1 (V.++) rs

        remDup :: Eq a => [(a, b, c)] -> [(a, b, c)]
        remDup [] = []
        remDup [x] = [x]
        remDup (x@(a1,_,_):y@(a2,_,_):xs) = if a1 == a2 then remDup (x : xs) else x : remDup (y:xs)


--- finds a permutation of input transforming an optimal circuit into 
--- one implementing the given function
retrieveP :: FuncId -> FuncId -> [(Var, Var)]
retrieveP equiv orig = zip [C1, C0, A, B] $ head [map fst p | p <- permutations allCombs, applyFunc equiv (map snd p) == orig]



--- swaps all variables in an expression according to a lookup table
permute :: Exp -> [(Var, Var)] -> Exp
permute exp env = r exp
    where
        r (BinApp g e1 e2) = BinApp g (r e1) (r e2)
        r (Not e) = Not (r e)
        r (Var x) = Var (fromJust $ lookup x env)
        r x = x


--- tests whether a circuit can be generated for given candidate code
integrationTest :: Vector (FuncId, Exp, Cost) -> B4num -> Bool
integrationTest rs b4 = cost /= -1 && origEval == exp'Eval  where
    origEval = eval (spec b4)
    exp'Eval = eval exp'
    exp' = permute exp perm
    perm = retrieveP evalres origEval
    evalres = eval exp
    (_, exp, cost) = V.head $ V.filter (\(fid, _, _) -> fid == origCan) (rs V.++ V.singleton (origCan, F, -1))
    origCan = canonize origEval

candidateCost :: Vector (FuncId, Exp, Cost) -> B4num -> Maybe Cost
candidateCost rs b4 = if cost /= -1 && origEval == exp'Eval then Just cost else Nothing  where
    origEval = eval (spec b4)
    exp'Eval = eval exp'
    exp' = permute exp perm
    perm = retrieveP evalres origEval
    evalres = eval exp
    (_, exp, cost) = V.head $ V.filter (\(fid, _, _) -> fid == origCan) (rs V.++ V.singleton (origCan, F, -1))
    origCan = canonize origEval

testProcess :: IO ()
testProcess = do
    let specExp = spec (Q0, Q0, Q0, Q2)
    let specF = eval specExp
    putStrLn $ "original R: " ++ show specF

    let specCan = canonize specF
    putStrLn $ "canonical R: " ++ show specCan

    let rs = augmentWithInputInversions s7res

    let (_, exp, cost) = V.head $ V.filter (\(fid, _, _) -> fid == specCan) (augmentWithInputInversions rs)



    -- canonical expression
    putStrLn $ "exp: " ++ show exp

    let invOrig = invertExp specExp [B]
    putStrLn $ "inverted original fid: " ++ show (canonize $ eval invOrig)

    let evalres =  eval exp
    putStrLn $ "evals to: " ++ show evalres
    putStrLn $ "canonically: " ++ show (canonize evalres)

    putStrLn $ "cost: " ++ show cost


    let perm = retrieveP evalres specF

    let exp' = permute exp perm

    print $ eval exp'
    print exp'


type BoolFunc = VarEnv -> Int
type ReplacableBoolFunc = GateEnv -> BoolFunc


dagToExp :: Dag Var -> Exp
dagToExp [] = error "empty dag"
dagToExp d = f size
    where
        f :: Int -> Exp
        f i = let (a, b) = d!!(size - i) in Repl (i-1) (g a) (g b)

        g :: DagNode Var -> Exp
        g (N x) = f x
        g (Inp v) = Var v

        size = length d


integrationTestsPassing :: Vector (FuncId, Exp, Cost) -> Int
integrationTestsPassing rs = length $ filter (integrationTest rs) allb4nums

-- evalDag :: Dag Var -> GateEnv -> VarEnv -> Int
-- evalDag

main :: IO ()
main = do
    -- testProcess

    -- let numGates = 6

    -- let allDags1 = take 100 $ genDags allVars 4
    -- let allDags2 = take 100 $ genDags allVars 5
    -- let allDags3 = take 100 $ genDags allVars 6

    -- let allDags = allDags3
    -- -- let allDags = concat [genDags allVars ng | ng <- [1..numGates]]

    -- -- putStrLn $ show (length allDags) ++ " dags in total"

    -- let exps = zip (repeat numGates) (map dagToExp allDags)

    let rs = s8_3res

    -- let rs = combineResults $ map allResults exps
    -- let rs = combineResults (map (`allResults` allCombs) exps ++ [s8_1res, s8_2res, s8_3res, s7res, s6res])
    -- let rs = augmentWithInputInversions $ combineResults (map (`allResults` allCombs) exps ++ [s8_1res, s8_2res, s8_3res, s7res, s6res])

    print $ V.length rs

    -- let testRes = map (candidateCost rs) allb4nums

    -- let succesfulTests = length $ filter isJust testRes

    -- let avgCost = fromIntegral (sum $ catMaybes testRes) / fromIntegral succesfulTests

    -- print succesfulTests
    -- print avgCost
