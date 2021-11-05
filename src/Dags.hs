{-# LANGUAGE ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Dags where 
import CommonTypes ( GenvEvaluable(..), Var, gateOp )

import qualified Data.Vector as V
import Data.Vector (Vector)
import Data.Bits ((.&.), complement)
import Data.Aeson
import Data.Aeson.Types (FromJSON)

data DagNode a = N Int | Inp a | Inv a deriving (Eq, Show)
newtype Dag a = Dag [(DagNode a, DagNode a)] deriving (Eq, Show)

allPairs :: (Ord a) => [DagNode a] -> [(DagNode a,DagNode a)]
allPairs xs =  [(N x, N y) | x <- ns, y <- ns, x < y] ++ [(Inp i, N x) | x <- ns, i <- inps] ++ [(Inp a, Inp b) | a <- inps, b <- inps, a < b]
    where inps = map (\(Inp x) -> x) $ filter (not.isN) xs
          ns   = map (\(N x) -> x) $ filter isN xs
          isN (N x) = True
          isN _ = False


genDags :: (Ord a) => [a] -> Int -> [Dag a]
genDags inps@[i1, i2, i3, i4] n = map Dag $ h n [(n-1), (n-2)..1] where
    h 1 _ = [[(Inp i1, Inp i2)]] -- due to permutation invariance N1 always uses A and B
    h 2 [] = [[(N 1,Inp i1), (Inp i1, Inp i2)], [(N 1,Inp i3), (Inp i1, Inp i2)], [(Inp i3, Inp i4), (Inp i1, Inp i2)]]
    h 2 _  = [[(N 1,Inp i1), (Inp i1, Inp i2)], [(N 1,Inp i3), (Inp i1, Inp i2)]]
    h n [] = [p:rg |  p <- allPairs (map Inp inps ++ map N [1..(n-1)]), rg <- h (n-1) []]
    h n (u:us) 
        | u == (n-1) = [(N u, i2):rg | i2 <- map Inp inps ++ map N [1..(n-2)], rg <- h (n-1) (filter (\i -> N i /= i2) us)]
        | otherwise  = [p:rg |  p <- allPairs (map Inp inps ++ map N [1..(n-1)]), rg <- h (n-1) (filter (\i -> N i /= snd p && N i /= fst p) (u:us))]
genDags _ _ = error "4 input values must be supplied"


instance Ord (Dag Var) where
    compare _ _ = EQ

instance GenvEvaluable (Dag Var) where
    eval (Dag dag) gEnv = 0xFFFF .&. head (foldr func [] dag) where
        func :: (DagNode Var, DagNode Var) -> [Int] -> [Int]
        func (x, y) prev = gateOp (gEnv V.! i) (val x) (val y) : prev
            where i = length prev
                  val (N no) = prev !! (i - no)
                  val (Inp var) = eval var V.empty
                  val (Inv var) = complement $ eval var V.empty

    invert (Dag dag) invs = Dag (map invertNode dag) where
        invertNode (a, b) = (invOne a, invOne b)
        invOne (N x) = N x
        invOne (Inp x) = if x `elem` invs then Inv x else Inp x
        invOne (Inv x) = if x `elem` invs then Inp x else Inv x

    permute (Dag dag) perm = Dag (map permuteNode dag) where
        permuteNode (a, b) = (permOne a, permOne b)
        permOne (N x) = N x
        permOne (Inp x) = Inp $ permute x perm
        permOne (Inv x) = Inv $ permute x perm

instance ToJSON (DagNode Var) where
    toJSON (N x)   = object ["t" .= ("N"   :: String), "v" .= x]
    toJSON (Inp v) = object ["t" .= ("Inp" :: String), "v" .= v]
    toJSON (Inv v) = object ["t" .= ("Inv" :: String), "v" .= v]

instance FromJSON (DagNode Var) where
    parseJSON = withObject "DagNode" $ \o -> do
        t <- o .: "t"
        case t of
            ("N"::String)   -> N <$> o   .: "v"
            "Inp"           -> Inp <$> o .: "v"
            "Inv"           -> Inv <$> o .: "v"
            _               -> fail "Invalid DagNode"


instance ToJSON (Dag Var) where
    toJSON (Dag dag) = toJSON dag

instance FromJSON (Dag Var) where
    parseJSON = withArray "Dag" $ \a -> Dag <$> mapM parseJSON (V.toList a)
