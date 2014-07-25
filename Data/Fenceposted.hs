{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric #-}
module Fenceposted
  ( Fenceposted(..)
  , singlePost
  , finalPost
  , postValuePairs
  , FencepostedF(..)
  , embed
  , project
  ) where

import qualified Data.Vector as Vec
import Data.Vector (Vector)
import Data.Traversable
import qualified Data.Foldable as F
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
import Data.Monoid
import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus), ap)
import Data.Functor.Classes (Eq1(eq1), Ord1(compare1), Read1(readsPrec1), Show1(showsPrec1))
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)

-- | @a@ values, separated by @post@s. There is one more @post@ than @a@.
data Fenceposted post a = Fenceposted (Vector (post, a)) post
  deriving (Show, Read, Eq, Ord, Functor, F.Foldable, Traversable, Data, Typeable, Generic)

instance (Eq post) => Eq1 (Fenceposted post) where
  eq1 = (==)

instance (Ord post) => Ord1 (Fenceposted post) where
  compare1 = compare

instance (Read post) => Read1 (Fenceposted post) where
  readsPrec1 = readsPrec

instance (Show post) => Show1 (Fenceposted post) where
  showsPrec1 = showsPrec

-- | @Lens\' ('Fenceposted' post a) post@
finalPost :: (Functor f) => (post -> f post) -> Fenceposted post a -> f (Fenceposted post a)
finalPost f (Fenceposted xs z) = Fenceposted xs <$> f z

-- | @Lens\' ('Fenceposted' post a) ('Vector' (post, a))@
postValuePairs :: (Functor f) => (Vector (post, a) -> f (Vector (post, a))) -> Fenceposted post a -> f (Fenceposted post a)
postValuePairs f (Fenceposted xs z) = flip Fenceposted z <$> f xs

singlePost :: post -> Fenceposted post a
singlePost = Fenceposted mempty

instance Bitraversable Fenceposted where
  bitraverse f g (Fenceposted xs z) = Fenceposted <$> traverse (bitraverse f g) xs <*> f z

instance Bifoldable Fenceposted where
  bifoldMap = bifoldMapDefault

instance Bifunctor Fenceposted where
  bimap = bimapDefault

uncons :: Vector a -> Maybe (a, Vector a)
uncons = bitraverse (Vec.!? 0) pure . Vec.splitAt 1

instance (Monoid post) => Monoid (Fenceposted post a) where
  mempty = singlePost mempty
  mappend (Fenceposted as aEnd) (Fenceposted bs bEnd) =
    case uncons bs of
      Just ((bStart, x), rest) -> Fenceposted (as <> pure (aEnd <> bStart, x) <> rest) bEnd
      Nothing -> Fenceposted as (aEnd <> bEnd)

fencepostedJoin :: (Monoid post) => Fenceposted post (Fenceposted post a) -> Fenceposted post a
fencepostedJoin (Fenceposted xs z) = F.foldMap (bifoldMap singlePost id) xs <> singlePost z

instance (Monoid post) => Monad (Fenceposted post) where
  return x = Fenceposted (pure (mempty, x)) mempty
  a >>= f = fencepostedJoin $ fmap f a

-- | A \'productish\' instance.
instance (Monoid post) => Applicative (Fenceposted post) where
  pure = return
  (<*>) = ap

instance (Monoid post) => Alternative (Fenceposted post) where
  empty = mempty
  (<|>) = mappend

instance (Monoid post) => MonadPlus (Fenceposted post) where
  mzero = mempty
  mplus = mappend

-- | Base functor for @'Fenceposted'@.
data FencepostedF post a r
  = FinalPost post
  | Panel post a r
  deriving (Eq, Show, Traversable, F.Foldable, Functor)

embed :: FencepostedF post a (Fenceposted post a) -> Fenceposted post a
embed (FinalPost post) = singlePost post
embed (Panel post x (Fenceposted xs z)) = Fenceposted (Vec.cons (post, x) xs) z

project :: Fenceposted post a -> FencepostedF post a (Fenceposted post a)
project (Fenceposted xs z) =
  case uncons xs of
    Just ((post, x), rest) -> Panel post x (Fenceposted rest z)
    Nothing -> FinalPost z
