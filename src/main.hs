{-# LANGUAGE ScopedTypeVariables #-}
{-# language BangPatterns #-}


{-# LANGUAGE ScopedTypeVariables #-}
{-# language BangPatterns #-}
import Data.Maybe (fromJust, Maybe, fromMaybe, isJust, catMaybes, isNothing, listToMaybe, mapMaybe)
import Data.List (intercalate, sort, nub, groupBy, permutations, group)
import Data.Bits (Bits (shiftR, shift, testBit), (.&.), (.|.), xor, complement, zeroBits, shiftL, bit, popCount)
import qualified Data.Vector as V
import Data.Vector (Vector)

import Numeric (showHex)

import Data.Aeson
import Data.Text.Lazy.IO as I
import qualified Data.Text.Lazy as I


import qualified Data.ByteString.Lazy as B

import qualified Data.Vector.Algorithms.Intro as V (sort, sortBy)

import Dags (genDags, Dag (Dag), DagNode (N, Inp, Inv))
import Specs (allb4nums, spec, cost, spec00, spec10, spec11, spec01)
import Exps
import CommonTypes
import Data.Aeson.Text (encodeToLazyText)
import Control.Exception (allowInterrupt)

import Latex ( latexTruthTable, circuitToTikzGraph, latexSpecTable )



allCombs :: VarEnv
allCombs = map (\v -> (v, eval v V.empty)) allVars


applyFunc :: FuncId -> [Int] -> Int
applyFunc f [a,b,c,d] = 0xFFFF .&. Prelude.foldl1 (.|.) [if testBit f (15 - lvl i) then bit i else 0 | i <- [0..15]]
    where lvl i =
           shift (a .&. bit i) (3-i) .|.
           shift (b .&. bit i) (2-i) .|.
           shift (c .&. bit i) (1-i) .|.
           shift (d .&. bit i) ( -i)
applyFunc f _ = error "error"


--- returns the lexicographically smallest permutation-equivalent function
smallestP :: FuncId -> FuncId
smallestP f = Prelude.minimum [applyFunc f p | p <- permutations vars]
    where vars = Prelude.map snd allCombs



--- generate all possible n-element arrays of gates
gateCombos :: GateEnv -> Int -> Vector GateEnv
gateCombos gs 1 = V.map V.singleton gs
gateCombos gs n = V.concatMap (\group -> V.map (`V.cons` group) gs) gEnvs
        where gEnvs = gateCombos gs (n-1)



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




toExp :: Circuit Exp -> Exp
toExp (exp, gEnv) = r exp
    where
        r (BinApp g e1 e2) = BinApp g (r e1) (r e2)
        r (Not e) = Not (r e)
        r (Repl x e1 e2) = BinApp (gEnv V.! x) (r e1) (r e2)
        r x = x

allResults :: (GenvEvaluable a, Ord a) => (Int, a) -> Vector (CircuitInfo a)
allResults (numGates, exp) = V.modify V.sort ar
    where
        ar =  V.map fromJust $ V.filter isJust (V.imap imapH tableForm)
            where imapH i = maybe Nothing (\(c, cst) -> Just (i, c, cst))

        tableForm = minCosts $ V.map compOne (gateCombos allGates numGates)

        compOne gEnv = (fid, (exp, gEnv), gcCost gEnv)
            where fid  = canonize $ mask $ eval exp gEnv
                  mask x = x .&. complement (shiftL (complement zeroBits) (2^4))
                  gcCost gEnv = V.sum $ V.map cost gEnv


        minCosts :: Vector (CircuitInfo a) -> Vector (Maybe (Circuit a, Cost))
        minCosts = V.foldl func (V.replicate (2^(2^4)) Nothing)
            where
                func :: Vector (Maybe (Circuit a, Cost)) -> CircuitInfo a -> Vector (Maybe (Circuit a, Cost))
                func vect (fid, c, cst) = if maybe True (\(c', cst')-> cst < cst') (vect V.! fid) then vect V.// [(fid, Just (c, cst))] else vect

canonize :: FuncId -> FuncId
canonize f = eqClass V.! f



augmentWithInputInversions :: (GenvEvaluable a, Ord a) => Vector (CircuitInfo a) -> Vector (CircuitInfo a)
augmentWithInputInversions rs = combineResults [V.concatMap (\res -> V.map (invertRes res) allVarLists) rs]

invertRes :: (GenvEvaluable a) => CircuitInfo a -> [Var] -> CircuitInfo a
invertRes (f, circ, cst) invs = (canonize $ uncurry eval circ', circ', cst + (3 * length invs))
    where circ' = invertCirc circ invs

invertCirc :: (GenvEvaluable a) => Circuit a -> [Var] -> Circuit a
invertCirc (circ, gEnv) invs = (invert circ invs, gEnv)


-- returns an input combo and a list of vars that were inverted
allVarLists :: V.Vector [Var]
allVarLists = V.fromList $ map gen [0..15]
    where
        gen :: Int -> [Var]
        gen x = [allVars!!i | i <- [0..3], testBit x i]




noInvertExpIsOriginal :: Bool
noInvertExpIsOriginal = and [uncurry eval circ == uncurry eval (invertCirc circ []) | circ <- map extractCirc (V.toList (allResults scheme4_7))]
    where extractCirc = \(_, c, _) -> c

twiceInvertExpIsOriginal :: Bool
twiceInvertExpIsOriginal = and [uncurry eval circ == uncurry eval (invertCirc (invertCirc circ invs) invs) | circ <- map extractCirc (V.toList (allResults scheme4_7)), invs <- V.toList allVarLists]
    where extractCirc= \(_, c, _) -> c






--- finds a permutation of input transforming an optimal circuit into 
--- one implementing the given function
retrieveP :: FuncId -> FuncId -> [(Var, Var)]
retrieveP equiv orig = zip [C1, C0, A, B] $ head [map fst p | p <- permutations allCombs, applyFunc equiv (map snd p) == orig]

eqClass :: Vector FuncId
eqClass = V.map smallestP (V.enumFromN 0 (2^16))

testEqClass :: Bool
testEqClass = V.all (\x -> eqClass V.! (eqClass V.! x) == eqClass V.! x) eqClass

eqClasses :: Vector FuncId
eqClasses = V.uniq $ V.modify V.sort eqClass






combineResults :: (Ord a) => [Vector (CircuitInfo a)] -> Vector (CircuitInfo a)
combineResults rs = V.fromList $ remDup sorted
    where
        sorted = V.toList $ V.modify V.sort concatenated
        concatenated = foldl1 (V.++) rs

        remDup :: Eq a => [(a, b, c)] -> [(a, b, c)]
        remDup [] = []
        remDup [x] = [x]
        remDup (x@(a1,_,_):y@(a2,_,_):xs) = if a1 == a2 then remDup (x : xs) else x : remDup (y:xs)


optimalCircuit :: (GenvEvaluable a) => Vector (CircuitInfo a) -> Exp -> Maybe (Circuit a, Cost)
optimalCircuit rs exp = maybeCirc where
    maybeCirc = (\(_, circ, cost) -> Just (origPerm circ, cost)) =<< maybeCI
    maybeCI = listToMaybe $ V.toList $ V.filter (\(fid, _, _) -> fid == origCan) rs
    origCan = canonize (eval exp V.empty)

    origPerm (dag, gEnv) = (permute dag (retrieveP (eval dag gEnv) (eval exp V.empty)), gEnv)



isExpAchievable :: Vector (CircuitInfo a) -> Exp -> Bool
isExpAchievable rs exp = not $ null $ V.filter (\(fid, _, _) -> fid == canonize (eval exp V.empty)) rs

numAchievable :: Vector (CircuitInfo a) -> Int
numAchievable rs = length $ filter (isExpAchievable rs) (map spec allb4nums)

dagToExp :: Dag Var -> Exp
dagToExp (Dag []) = error "empty dag"
dagToExp (Dag d) = f size
    where
        f :: Int -> Exp
        f i = let (a, b) = d!!(size - i) in Repl (i-1) (g a) (g b)

        g :: DagNode Var -> Exp
        g (N x) = f x
        g (Inp v) = Var v
        g (Inv v) = Not (Var v)
        size = length d

every n xs = case drop (n-1) xs of
              y : ys -> y : every n ys
              [] -> []


createResultDatabase = do
    let smallDags = concat [zip (repeat ng) (genDags allVars ng) | ng <- [1..4]]
    print $ length smallDags
    let dags5 = zip (repeat 5) $ every 10 $ genDags allVars 5
    print $ length dags5
    let dags6 = zip (repeat 6) $ every 170 $ genDags allVars 6
    print $ length dags6
    let dags7 = zip (repeat 7) $ every 3000 $ genDags allVars 7
    print $ length dags7
    let dags8 = zip (repeat 8) $ every 3000000 $ genDags allVars 8
    print $ length dags8

    let dags = concat [smallDags, dags5, dags6, dags7, dags8]

    let rs = combineResults $ map allResults dags
    print $ V.length rs

    let rs2 = augmentWithInputInversions rs

    print $ V.length rs2
    testAllCandidates rs2
    I.writeFile "rs_dump.json" (encodeToLazyText rs2)


testAllCandidates :: Vector (CircuitInfo (Dag Var)) -> IO()
testAllCandidates rs = do
    let allSpecExps = map spec allb4nums

    let successes = mapMaybe (optimalCircuit rs) allSpecExps

    let no =  length successes
    print no

    let totalCost = sum $ map snd successes

    print $ fromIntegral totalCost /  fromIntegral no


digiShow :: Gate -> String
digiShow And = "AND"
digiShow Nand = "NAND"
digiShow Or = "OR"
digiShow Xor = "XOR"
digiShow Nor = "NOR"
digiShow Xnor = "XNOR"


makeDigisimGraph :: Int -> Circuit (Dag Var) -> String
makeDigisimGraph num (Dag dag, gEnv) = header ++ dagNodesDescr ++ footer   where
        size = length dag

        (d3, d2, d1, d0) = allb4nums !! num
        header = "// AUTOMATICALLY GENERATED, DO NOT EDIT\n"
               ++"// My solution to the Hardware Coursework.\n"
               ++"// My number is " ++ show num ++ " or " ++ show d3 ++ show d2 ++ show d1 ++ show d0 ++ " in base 4.\n"
               ++"// My functions are: (00)= " ++ show (spec00 d3) ++ "  (01)= " ++ show (spec01 d2) ++ "  (10)= " ++ show (spec10 d1) ++ "  (11)= " ++ show (spec11 d0) ++ "\n"
               ++"<circuit>\n"
               ++"<number>" ++ show num ++ "</number>\n"
               ++"<inputs>  C1 C0 A B  </inputs>\n"
               ++"<outputs>  N" ++ show size ++  "  </outputs>\n"
               ++"<gates>\n"

        footer = "</gates>\n</circuit>\n"

        dagNodesDescr :: String
        dagNodesDescr = concatMap h (zip [size, size-1..(-1)] dag)
            where h (i, (v1, v2)) = "    "
                                 ++ digiShow (gEnv V.! (i - 1)) 
                                 ++ "(N" ++ show i ++ ", " 
                                 ++ helper v1 
                                 ++ ", " 
                                 ++ helper v2
                                 ++")\n"

        helper :: DagNode Var -> String
        helper (N i) = "N" ++ show i
        helper (Inp v) = show v
        helper (Inv v) = show v ++ "'"

        invertionsDescr :: String
        invertionsDescr = concatMap (\v -> "    INVERTER(" ++ show v ++ "', " ++ show v ++ ")\n") allInvertions

        allInvertions :: [Var]
        allInvertions = nub $ concatMap (\(v1, v2) -> h v1 ++ h v2) dag
            where h (Inv x) = [x]
                  h _ = []



main :: IO ()
main = do
    createResultDatabase
    bs <- B.readFile "rs_dump.json"
    let rs = fromJust (decode bs) :: Vector (CircuitInfo (Dag Var))

    print $ V.length rs

    testAllCandidates rs

    let decNum = 56
    let num@(d3, d2, d1, d0) = allb4nums !! decNum
    let numShow = show d3 ++ show d2 ++ show d1 ++ show d0
    let specExp = spec num


    let (circ, cost) = fromJust $ optimalCircuit rs specExp

    let specTable = I.pack $ latexSpecTable num
    let truthTable = I.pack $ latexTruthTable num
    let graph = I.pack $ circuitToTikzGraph circ

    texFile <- I.readFile "writeup/main.tex"

    let r1 = I.replace (I.pack "%GRAPH") graph texFile 
    let r2 = I.replace (I.pack "%TRUTHTABLE") truthTable r1 
    let r3 = I.replace (I.pack "%SPECTABLE") specTable r2 
    let r4 = I.replace (I.pack "DECNUM") (I.pack $ show decNum) r3 
    let r5 = I.replace (I.pack "B4NUM") (I.pack numShow) r4
    let r6 = I.replace (I.pack "TOTALCOST") (I.pack $ show cost) r5

    I.writeFile "writeup/filled.tex" r6
    I.writeFile "hardware.txt" (I.pack $ makeDigisimGraph decNum circ)

