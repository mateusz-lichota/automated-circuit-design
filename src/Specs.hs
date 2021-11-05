module Specs where

import Exps
import Data.List (intercalate)
import CommonTypes (Gate(..), Cost, Var(..))

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
ppTable t = unlines $ map (intercalate " | ") t


cost :: Gate -> Cost
cost And = 6
cost Nand = 4
cost Or = 6
cost Nor = 4
cost Xor = 8
cost Xnor = 8
