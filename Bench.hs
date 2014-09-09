import Criterion.Main
import Data.Fenceposted
import qualified Data.Foldable as F
import Data.List
import Data.Monoid
import qualified Data.Semigroup as Semi

foo :: Int -> Int -> Fenceposted String String
foo n i = Fenceposted [("a", show n ++ show i)] "b"

leftwardM :: Int -> Fenceposted String String
leftwardM n = foldl' (\ acc i -> acc `mappend` foo n i) mempty [0..n]

rightwardM :: Int -> Fenceposted String String
rightwardM n = foldr (\ i acc -> foo n i `mappend` acc) mempty [0..n]

leftwardS :: Int -> Fenceposted String String
leftwardS n = foldl' (\ acc i -> acc Semi.<> foo n i) mempty [0..n]

rightwardS :: Int -> Fenceposted String String
rightwardS n = foldr (\ i acc -> foo n i Semi.<> acc) mempty [0..n]

main :: IO ()
main = defaultMain
  [ bgroup "foldl' Monoid"
    [ bench (show n) $ whnf (length . F.toList . leftwardM) n | n <- map (* 500) [1..4] ]
  , bgroup "foldr Monoid"
    [ bench (show n) $ whnf (length . F.toList . rightwardM) n | n <- map (* 3000) [1..4] ]
  , bgroup "foldl' Semigroup"
    [ bench (show n) $ whnf (length . F.toList . leftwardS) n | n <- map (* 500) [1..4] ]
  , bgroup "foldr Semigroup"
    [ bench (show n) $ whnf (length . F.toList . rightwardS) n | n <- map (* 3000) [1..4] ]
  ]
