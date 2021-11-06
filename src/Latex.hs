module Latex where

import qualified Data.Vector as V
import CommonTypes
import Specs
import Exps
import Dags
import Data.List (nub, intercalate)
import Data.Bits (testBit)

latexTruthTable :: B4num -> String
latexTruthTable b4@(d3, d2, d1, d0) = unlines b
    where
        e = spec b4
        ds = [spec00 d3, spec01 d2, spec10 d1, spec11 d0]
        b = zipWith (\i l -> (if i `mod` 4 == 1 then "\\hline \n" ++ l ++ "\\multirow{4}{*}{" ++ show (ds!!(i `div` 4)) ++ "} " else l) ++ "\\\\") [0..] a
        a =  head lines : zipWith (\i l -> l ++ (if testBit fid (15 - i) then "1" else "0") ++ " & ") [0..] (tail lines)
        fid = eval e V.empty
        lines = [
            "$C_1$ & $C_2$ & $A$ & $B$ & $R$ & Function",
            "0     &  0    &  0  &  0  & ",
            "0     &  0    &  0  &  1  & ",
            "0     &  0    &  1  &  0  & ",
            "0     &  0    &  1  &  1  & ",
            "0     &  1    &  0  &  0  & ",
            "0     &  1    &  0  &  1  & ",
            "0     &  1    &  1  &  0  & ",
            "0     &  1    &  1  &  1  & ",
            "1     &  0    &  0  &  0  & ",
            "1     &  0    &  0  &  1  & ",
            "1     &  0    &  1  &  0  & ",
            "1     &  0    &  1  &  1  & ",
            "1     &  1    &  0  &  0  & ",
            "1     &  1    &  0  &  1  & ",
            "1     &  1    &  1  &  0  & ",
            "1     &  1    &  1  &  1  & "]

circuitToTikzGraph :: Circuit (Dag Var) -> String
circuitToTikzGraph (Dag dag, gEnv) = dagNodesDescr ++ invertionsDescr ++ dagEdgesDescr ++ "N" ++ show size ++ " -> R\n"
    where
        size = length dag

        dagNodesDescr :: String
        dagNodesDescr = concatMap h (zip [size, size-1..(-1)] (V.toList gEnv))
            where h (i, g) = helper (N i) ++ " [" ++ show (gEnv V.! (i - 1)) ++ " gate US, draw],\n"

        helper :: DagNode Var -> String
        helper (N i) = "N" ++ show i
        helper (Inp v) = show v
        helper (Inv v) = show v ++ "'"

        invertionsDescr :: String
        invertionsDescr = concatMap (\v -> show v ++ " -- " ++ show v ++ "' [not gate US, draw],\n") allInvertions

        allInvertions :: [Var]
        allInvertions = nub $ concatMap (\(v1, v2) -> h v1 ++ h v2) dag
            where h (Inv x) = [x]
                  h _ = []


        dagEdgesDescr :: String
        dagEdgesDescr = concatMap h (zip [size, size-1..(-1)] dag)
            where h (i, (v1, v2)) = helper v1 ++ " -- " ++ helper (N i) ++ ",\n"
                                 ++ helper v2 ++ " -- " ++ helper (N i) ++ ",\n"


latexSpecTable :: B4num -> String
latexSpecTable (d3, d2, d1, d0) = unlines $ map ((++"\\\\ \\hline").intercalate " & ")
    [
        [     "Digits "     ,"$C_1$","$C_2$",     "Output"    ],
        ["$D_3 = " ++ show d3 ++ "$"," 0 "," 0 ", show (spec00 d3)],
        ["$D_2 = " ++ show d2 ++ "$"," 0 "," 1 ", show (spec01 d2)],
        ["$D_1 = " ++ show d1 ++ "$"," 1 "," 0 ", show (spec10 d1)],
        ["$D_0 = " ++ show d0 ++ "$"," 1 "," 1 ", show (spec11 d0)]
    ]


