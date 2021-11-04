{-# LANGUAGE ScopedTypeVariables #-}

module Dags where 

data DagNode a = N Int | Inp a deriving (Eq, Show)
type Dag a = [(DagNode a, DagNode a)]

allPairs :: (Ord a) => [DagNode a] -> [(DagNode a,DagNode a)]
allPairs xs =  [(N x, N y) | x <- ns, y <- ns, x < y] ++ [(Inp i, N x) | x <- ns, i <- inps] ++ [(Inp a, Inp b) | a <- inps, b <- inps, a < b]
    where inps = map (\(Inp x) -> x) $ filter (not.isN) xs
          ns   = map (\(N x) -> x) $ filter isN xs
          isN (N x) = True
          isN (Inp x) = False


genDags :: (Ord a) => [a] -> Int -> [Dag a]
genDags inps@[i1, i2, i3, i4] n = h n [(n-1), (n-2)..1] where
    h 1 _ = [[(Inp i1, Inp i2)]] -- due to permutation invariance N1 always uses A and B
    h 2 [] = [[(N 1,Inp i1), (Inp i1, Inp i2)], [(N 1,Inp i3), (Inp i1, Inp i2)], [(Inp i3, Inp i4), (Inp i1, Inp i2)]]
    h 2 _  = [[(N 1,Inp i1), (Inp i1, Inp i2)], [(N 1,Inp i3), (Inp i1, Inp i2)]]
    h n [] = [p:rg |  p <- allPairs (map Inp inps ++ map N [1..(n-1)]), rg <- h (n-1) []]
    h n (u:us) 
        | u == (n-1) = [(N u, i2):rg | i2 <- map Inp inps ++ map N [1..(n-2)], rg <- h (n-1) (filter (\i -> N i /= i2) us)]
        | otherwise  = [p:rg |  p <- allPairs (map Inp inps ++ map N [1..(n-1)]), rg <- h (n-1) (filter (\i -> N i /= snd p && N i /= fst p) (u:us))]

genDags _ _ = error "4 input values must be supplied"
