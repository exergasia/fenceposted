{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric #-}
module Data.Fenceposted1
  ( Fenceposted1(..)
  , panel
  , joinPosts
  ) where

import Data.Traversable
import qualified Data.Foldable as F
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
import Control.Applicative
import Data.Functor.Classes (Eq1(eq1), Ord1(compare1), Read1(readsPrec1), Show1(showsPrec1))
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)
import Data.Semigroup
import Data.Semigroup.Bifoldable
import Data.Functor.Apply
import qualified Data.Functor.Bind as Bind
import Data.Functor.Alt
import Data.Semigroup.Bitraversable
import Data.List.NonEmpty (NonEmpty((:|)), (<|))
import Data.Semigroup.Foldable
import Data.Semigroup.Traversable

-- | @a@ values, separated by @post@s. There is one more @post@ than @a@ and
-- at least one @a@.
data Fenceposted1 post a = Fenceposted1 (NonEmpty (post, a)) post
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance (Eq post) => Eq1 (Fenceposted1 post) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord post) => Ord1 (Fenceposted1 post) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read post) => Read1 (Fenceposted1 post) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show post) => Show1 (Fenceposted1 post) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

-- | Add a new \'post\' and a \'panel\' at the left.
panel :: post -> a -> Fenceposted1 post a -> Fenceposted1 post a
panel post x (Fenceposted1 xs z) = Fenceposted1 ((post, x) <| xs) z
{-# INLINE CONLIKE panel #-}

joinPosts :: Fenceposted1 (Fenceposted1 post a) a -> Fenceposted1 post a
joinPosts (Fenceposted1 xs z) = F.foldr f z xs
  where
    f (Fenceposted1 as az, x) (Fenceposted1 bs bz) = Fenceposted1 (as <> ((az, x) <| bs)) bz
{-# INLINE joinPosts #-}

instance Bitraversable1 Fenceposted1 where
  bitraverse1 f g = \ (Fenceposted1 xs z) -> Fenceposted1 <$> traverse1 (bitraverse1 f g) xs <.> f z
  {-# INLINE bitraverse1 #-}

instance Bitraversable Fenceposted1 where
  bitraverse f g = unwrapApplicative . bitraverse1 (WrapApplicative . f) (WrapApplicative . g)
  {-# INLINE bitraverse #-}

instance Bifoldable1 Fenceposted1 where
  bifoldMap1 f g = bifoldMap1Default f g
  {-# INLINE bifoldMap1 #-}

instance Bifoldable Fenceposted1 where
  bifoldMap f g = bifoldMapDefault f g
  {-# INLINE bifoldMap #-}

instance Bifunctor Fenceposted1 where
  bimap f g = bimapDefault f g
  {-# INLINE bimap #-}

instance Traversable1 (Fenceposted1 a) where
  traverse1 f = \ (Fenceposted1 xs z) -> flip Fenceposted1 z <$> traverse1 (traverse1 f) xs
  {-# INLINE traverse1 #-}

instance Traversable (Fenceposted1 a) where
  traverse f = bitraverse pure f
  {-# INLINE traverse #-}

instance Foldable1 (Fenceposted1 a) where
  foldMap1 f = foldMap1Default f
  {-# INLINE foldMap1 #-}

instance F.Foldable (Fenceposted1 a) where
  foldMap f = foldMapDefault f
  {-# INLINE F.foldMap #-}

instance Functor (Fenceposted1 a) where
  fmap f = fmapDefault f
  {-# INLINE fmap #-}

append :: (post -> post -> post) -> Fenceposted1 post a -> Fenceposted1 post a -> Fenceposted1 post a
append op (Fenceposted1 as aEnd) (Fenceposted1 ((bStart, b) :| bs) bEnd) =
  Fenceposted1 (as <> ((aEnd `op` bStart, b) :| bs)) bEnd
{-# INLINE append #-}

instance (Semigroup post) => Semigroup (Fenceposted1 post a) where
  (<>) = append (<>)
  {-# INLINE (<>) #-}

-- | A \'productish\' instance.
instance (Semigroup post) => Apply (Fenceposted1 post) where
  f <.> a = Bind.join $ (<$> a) <$> f
  {-# INLINE (<.>) #-}

instance (Monoid post) => Applicative (Fenceposted1 post) where
  pure x = Fenceposted1 (pure (mempty, x)) mempty
  {-# INLINE pure #-}
  f <*> a = first unwrapMonoid $ first WrapMonoid f <.> first WrapMonoid a
  {-# INLINE (<*>) #-}

prependPost :: (Semigroup post) => post -> Fenceposted1 post a -> Fenceposted1 post a
prependPost post1 (Fenceposted1 ((post2, a) :| as) z) = Fenceposted1 ((post1 <> post2, a) :| as) z

appendPost :: (Semigroup post) => Fenceposted1 post a -> post -> Fenceposted1 post a
appendPost (Fenceposted1 xs z) post = Fenceposted1 xs $ z <> post

instance (Semigroup post) => Bind.Bind (Fenceposted1 post) where
  join = \ (Fenceposted1 xs z) -> appendPost (foldMap1 (uncurry prependPost) xs) z
  {-# INLINE join #-}

instance (Monoid post) => Monad (Fenceposted1 post) where
  return = pure
  {-# INLINE return #-}
  a >>= f = first unwrapMonoid $ Bind.join $ fmap (first WrapMonoid . f) (first WrapMonoid a)
  {-# INLINE (>>=) #-}

instance (Semigroup post) => Alt (Fenceposted1 post) where
  (<!>) = (<>)
  {-# INLINE (<!>) #-}

-- XXX: Comonad and allies; dep of 'profunctors' so it's not sucking in more deps.
