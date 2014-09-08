{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Data.TaggedTree
 ( TaggedTree(..)
 , _TaggedTree
 , subtree
 , foldTaggedTree
 , nullTaggedTree
 , embedTaggedTree
 , projectTaggedTree
 ) where

import Data.Fenceposted
import Data.Monoid
import Control.Applicative
import qualified Data.Foldable as F
import Data.Traversable
import Data.Bitraversable
import Data.Bifoldable
import Data.Semigroup.Bitraversable
import Data.Semigroup.Bifoldable
import Data.Functor.Apply
import Data.Semigroup hiding ((<>))
import qualified Data.Semigroup as Semi
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable
import Data.Bifunctor
import qualified Data.Functor.Bind as Bind
import GHC.Generics
import Data.Data
import Data.Profunctor

data TaggedTree tag a
      = TaggedTree { unTaggedTree :: Fenceposted a (tag, TaggedTree tag a) }
    deriving (Eq, Ord, Read, Show, Generic, Data, Typeable)

-- | @_TaggedTree :: Iso (Fenceposted a (tag, TaggedTree tag a)) (Fenceposted a' (tag', TaggedTree tag' a')) (TaggedTree tag a) (TaggedTree tag' a')
_TaggedTree :: (Profunctor p, Functor f) => p (TaggedTree tag a) (f (TaggedTree tag' a')) -> p (Fenceposted a (tag, TaggedTree tag a)) (f (Fenceposted a' (tag', TaggedTree tag' a')))
_TaggedTree = dimap TaggedTree (fmap unTaggedTree)
{-# INLINE _TaggedTree #-}

foldTaggedTree :: (Fenceposted a (tag, r) -> r) -> TaggedTree tag a -> r
foldTaggedTree f = f . fmap (fmap (foldTaggedTree f)) . projectTaggedTree
{-# INLINE foldTaggedTree #-}

projectTaggedTree :: TaggedTree tag a -> Fenceposted a (tag, TaggedTree tag a)
projectTaggedTree = unTaggedTree
{-# INLINE projectTaggedTree #-}

embedTaggedTree :: Fenceposted a (tag, TaggedTree tag a) -> TaggedTree tag a
embedTaggedTree = TaggedTree
{-# INLINE embedTaggedTree #-}

nullTaggedTree :: (Eq a, Monoid a) => TaggedTree tag a -> Bool
nullTaggedTree t =
  case projectFenceposted $ unTaggedTree t of
    FinalPost x -> x == mempty
    Panel _ _ _ -> False
{-# INLINE nullTaggedTree #-}

instance (Semigroup a) => Semigroup (TaggedTree tag a) where
  a <> b = TaggedTree $ unTaggedTree a Semi.<> unTaggedTree b
  {-# INLINE (Semi.<>) #-}

instance (Monoid a) => Monoid (TaggedTree tag a) where
  mempty = TaggedTree mempty
  {-# INLINE mempty #-}
  mappend a b = fmap unwrapMonoid $ fmap WrapMonoid a Semi.<> fmap WrapMonoid b
  {-# INLINE mappend #-}

instance Bitraversable1 TaggedTree where
  bitraverse1 f g = foldTaggedTree (fmap embedTaggedTree . bitraverse1 g (bitraverse1 f id))
  {-# INLINE bitraverse1 #-}

instance Bifoldable1 TaggedTree where
  bifoldMap1 = bifoldMap1Default
  {-# INLINE bifoldMap1 #-}

instance Bitraversable TaggedTree where
  bitraverse f g = unwrapApplicative . bitraverse1 (WrapApplicative . f) (WrapApplicative . g)
  {-# INLINE bitraverse #-}

instance Traversable1 (TaggedTree tag) where
  traverse1 f = foldTaggedTree (fmap embedTaggedTree . bitraverse1 f sequence1)
  {-# INLINE traverse1 #-}

instance Foldable1 (TaggedTree tag) where
  foldMap1 = foldMap1Default
  {-# INLINE foldMap1 #-}

instance Traversable (TaggedTree tag) where
  traverse = bitraverse pure
  {-# INLINE traverse #-}

instance Bifoldable TaggedTree where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

instance F.Foldable (TaggedTree tag) where
  foldMap = foldMapDefault
  {-# INLINE foldMap #-}

instance Bifunctor TaggedTree where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance Functor (TaggedTree tag) where
  fmap = fmapDefault
  {-# INLINE fmap #-}

subtree :: (Monoid a) => tag -> TaggedTree tag a -> TaggedTree tag a
subtree tag f = TaggedTree (pure (tag, f))
{-# INLINE subtree #-}

instance Bind.Bind (TaggedTree tag) where
  join = foldTaggedTree (TaggedTree . joinPosts . first unTaggedTree)
  {-# INLINE join #-}

instance Apply (TaggedTree tag) where
  f <.> a = Bind.join $ (<$> a) <$> f
  {-# INLINE (<.>) #-}

instance Applicative (TaggedTree tag) where
  pure = TaggedTree . fencepost
  {-# INLINE pure #-}
  (<*>) = (<.>)
  {-# INLINE (<*>) #-}

instance Monad (TaggedTree tag) where
  return = pure
  {-# INLINE return #-}
  a >>= f = Bind.join $ f <$> a
  {-# INLINE (>>=) #-}
