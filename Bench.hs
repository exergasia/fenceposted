import Criterion.Main
import Data.Fenceposted
import qualified Data.Foldable as F
import Control.Applicative
import Data.List
import Data.Monoid

leftward :: Int -> Fenceposted () Int
leftward n = foldl' (\ acc i -> pure i `mappend` acc) mempty [0..n]

rightward :: Int -> Fenceposted () Int
rightward n = foldr (\ i acc -> acc `mappend` pure i) mempty [0..n]

main = defaultMain
  [ bgroup "foldl'"
    [ bench (show n) $ whnf (length . F.toList . leftward) n | n <- map (* 10) [1..25] ]
  , bgroup "foldr"
    [ bench (show n) $ whnf (length . F.toList . rightward) n | n <- map (* 10) [1..25] ]
  ]
