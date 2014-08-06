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

foldTaggedTree :: (Fenceposted a (tag, r) -> r) -> TaggedTree tag a -> r
foldTaggedTree f = f . fmap (fmap (foldTaggedTree f)) . projectTaggedTree

projectTaggedTree :: TaggedTree tag a -> Fenceposted a (tag, TaggedTree tag a)
projectTaggedTree = unTaggedTree

embedTaggedTree :: Fenceposted a (tag, TaggedTree tag a) -> TaggedTree tag a
embedTaggedTree = TaggedTree

nullTaggedTree :: (Eq a, Monoid a) => TaggedTree tag a -> Bool
nullTaggedTree t =
  case projectFenceposted $ unTaggedTree t of
    FinalPost x -> x == mempty
    Panel _ _ _ -> False

instance (Semigroup a) => Semigroup (TaggedTree tag a) where
  a <> b = TaggedTree $ unTaggedTree a Semi.<> unTaggedTree b

instance (Monoid a) => Monoid (TaggedTree tag a) where
  mempty = TaggedTree mempty
  mappend a b = fmap unwrapMonoid $ fmap WrapMonoid a Semi.<> fmap WrapMonoid b

instance Bitraversable1 TaggedTree where
  bitraverse1 f g = foldTaggedTree (fmap embedTaggedTree . bitraverse1 g (bitraverse1 f id))

instance Bifoldable1 TaggedTree where
  bifoldMap1 = bifoldMap1Default

instance Bitraversable TaggedTree where
  bitraverse f g = unwrapApplicative . bitraverse1 (WrapApplicative . f) (WrapApplicative . g)

instance Traversable1 (TaggedTree tag) where
  traverse1 f = foldTaggedTree (fmap embedTaggedTree . bitraverse1 f sequence1)

instance Foldable1 (TaggedTree tag) where
  foldMap1 = foldMap1Default

instance Traversable (TaggedTree tag) where
  traverse = bitraverse pure

instance Bifoldable TaggedTree where
  bifoldMap = bifoldMapDefault

instance F.Foldable (TaggedTree tag) where
  foldMap = foldMapDefault

instance Bifunctor TaggedTree where
  bimap = bimapDefault

instance Functor (TaggedTree tag) where
  fmap = fmapDefault

subtree :: (Monoid a) => tag -> TaggedTree tag a -> TaggedTree tag a
subtree tag f = TaggedTree (pure (tag, f))

instance Bind.Bind (TaggedTree tag) where
  join = foldTaggedTree (TaggedTree . joinPosts . first unTaggedTree)

instance Apply (TaggedTree tag) where
  f <.> a = Bind.join $ (<$> a) <$> f

instance Applicative (TaggedTree tag) where
  pure = TaggedTree . fencepost
  (<*>) = (<.>)

instance Monad (TaggedTree tag) where
  return = pure
  a >>= f = Bind.join $ f <$> a
