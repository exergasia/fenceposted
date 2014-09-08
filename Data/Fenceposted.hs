{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric #-}
module Data.Fenceposted
  ( Fenceposted(..)
  , fencepost
  , panel
  , posts
  , joinPosts
  , FencepostedF(..)
  , fencepostedF
  , tritraverse1FencepostedF
  , embedFenceposted
  , projectFenceposted
  , fencepostZipWith
  , ZipFenceposted(..)
  ) where

import Data.Traversable
import qualified Data.Foldable as F
import Data.Bitraversable
import Data.Bifoldable
import Data.Bifunctor
import Data.Monoid
import Control.Applicative
import Control.Monad (MonadPlus(mzero, mplus))
import Data.Functor.Classes (Eq1(eq1), Ord1(compare1), Read1(readsPrec1), Show1(showsPrec1))
import Data.Typeable (Typeable)
import Data.Data (Data)
import GHC.Generics (Generic)
import qualified Data.Semigroup as Semi
import Data.Semigroup hiding ((<>))
import Data.Semigroup.Bifoldable
import Data.Functor.Apply
import qualified Data.Functor.Bind as Bind
import Data.Functor.Alt
import Data.Semigroup.Bitraversable

-- | @a@ values, separated by @post@s. There is one more @post@ than @a@.
--
-- >>> bitraverse_ putStrLn (putStrLn . (++ "!!!") . show) $ fencepost "foo" <> pure True <> fencepost "bar"
-- foo
-- True!!!
-- bar
data Fenceposted post a = Fenceposted [(post, a)] post
  deriving (Show, Read, Eq, Ord, Functor, F.Foldable, Traversable, Data, Typeable, Generic)

instance (Eq post) => Eq1 (Fenceposted post) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord post) => Ord1 (Fenceposted post) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read post) => Read1 (Fenceposted post) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show post) => Show1 (Fenceposted post) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

-- | @posts :: Traversal1 (Fenceposted post a) (Fenceposted post\' a) post post\'@
posts :: (Apply f) => (post -> f post') -> Fenceposted post a -> f (Fenceposted post' a)
posts f (Fenceposted xs z) = F.foldr (\ (post, x) acc -> flip panel x <$> f post <.> acc) (fencepost <$> f z) xs
{-# INLINE posts #-}

-- | A single terminal fencepost.
fencepost :: post -> Fenceposted post a
fencepost = Fenceposted mempty
{-# INLINE fencepost #-}

-- | Add a new \'post\' and a \'panel\' at the left.
panel :: post -> a -> Fenceposted post a -> Fenceposted post a
panel post x (Fenceposted xs z) = Fenceposted ((post, x) : xs) z
{-# INLINE panel #-}

joinPosts :: Fenceposted (Fenceposted post a) a -> Fenceposted post a
joinPosts (Fenceposted xs z) = foldr f z xs
  where
    f (Fenceposted as az, x) (Fenceposted bs bz) = Fenceposted (as <> ((az, x):bs)) bz
{-# INLINE joinPosts #-}

instance Bitraversable1 Fenceposted where
  bitraverse1 f g (Fenceposted xs z) = F.foldr (liftF2 (uncurry panel)) (fencepost <$> f z) $ bitraverse1 f g <$> xs
  {-# INLINE bitraverse1 #-}

instance Bitraversable Fenceposted where
  bitraverse f g = unwrapApplicative . bitraverse1 (WrapApplicative . f) (WrapApplicative . g)
  {-# INLINE bitraverse #-}

instance Bifoldable1 Fenceposted where
  bifoldMap1 = bifoldMap1Default
  {-# INLINE bifoldMap1 #-}

instance Bifoldable Fenceposted where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

instance Bifunctor Fenceposted where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance (Semigroup post) => Semigroup (Fenceposted post a) where
  Fenceposted as aEnd <> b =
    case projectFenceposted b of
      Panel bStart x (Fenceposted rest bEnd) -> Fenceposted (as <> pure (aEnd Semi.<> bStart, x) <> rest) bEnd
      FinalPost bEnd -> Fenceposted as (aEnd Semi.<> bEnd)
  {-# INLINE (Semi.<>) #-}

instance (Monoid post) => Monoid (Fenceposted post a) where
  mempty = fencepost mempty
  {-# INLINE mempty #-}
  mappend a b = first unwrapMonoid $ first WrapMonoid a Semi.<> first WrapMonoid b
  {-# INLINE mappend #-}

-- | A \'productish\' instance.
instance (Semigroup post) => Apply (Fenceposted post) where
  f <.> a = Bind.join $ (<$> a) <$> f
  {-# INLINE (<.>) #-}

instance (Monoid post) => Applicative (Fenceposted post) where
  pure x = Fenceposted (pure (mempty, x)) mempty
  {-# INLINE pure #-}
  f <*> a = first unwrapMonoid $ first WrapMonoid f <.> first WrapMonoid a
  {-# INLINE (<*>) #-}

instance (Semigroup post) => Bind.Bind (Fenceposted post) where
  join = bifoldMap1 fencepost id
  {-# INLINE join #-}

instance (Monoid post) => Monad (Fenceposted post) where
  return = pure
  {-# INLINE return #-}
  a >>= f = first unwrapMonoid $ Bind.join $ fmap (first WrapMonoid . f) (first WrapMonoid a)
  {-# INLINE (>>=) #-}

instance (Semigroup post) => Alt (Fenceposted post) where
  (<!>) = (Semi.<>)
  {-# INLINE (<!>) #-}

instance (Monoid post) => Alternative (Fenceposted post) where
  empty = mempty
  {-# INLINE empty #-}
  (<|>) = mappend
  {-# INLINE (<|>) #-}

instance (Monoid post) => MonadPlus (Fenceposted post) where
  mzero = mempty
  {-# INLINE mzero #-}
  mplus = mappend
  {-# INLINE mplus #-}

-- | Base functor for @'Fenceposted'@.
data FencepostedF post a r
  = FinalPost post
  | Panel post a r
  deriving (Eq, Ord, Show, Read, Traversable, F.Foldable, Functor, Data, Typeable, Generic)

instance (Eq post, Eq a) => Eq1 (FencepostedF post a) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord post, Ord a) => Ord1 (FencepostedF post a) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read post, Read a) => Read1 (FencepostedF post a) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show post, Show a) => Show1 (FencepostedF post a) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

fencepostedF :: (post -> x) -> (post -> a -> r -> x) -> FencepostedF post a r -> x
fencepostedF f _ (FinalPost post) = f post
fencepostedF _ g (Panel post a r) = g post a r
{-# INLINE fencepostedF #-}

tritraverse1FencepostedF :: (Apply f) => (post -> f post') -> (a -> f a') -> (r -> f r') -> FencepostedF post a r -> f (FencepostedF post' a' r')
tritraverse1FencepostedF f g h = fencepostedF (fmap FinalPost . f) (\ post a r -> Panel <$> f post <.> g a <.> h r)
{-# INLINE tritraverse1FencepostedF #-}

embedFenceposted :: FencepostedF post a (Fenceposted post a) -> Fenceposted post a
embedFenceposted = fencepostedF fencepost panel
{-# INLINE embedFenceposted #-}

projectFenceposted :: Fenceposted post a -> FencepostedF post a (Fenceposted post a)
projectFenceposted (Fenceposted xs z) =
  case xs of
    (post, x) : rest -> Panel post x (Fenceposted rest z)
    [] -> FinalPost z
{-# INLINE projectFenceposted #-}

-- | Alternative \'zippish\' @Apply@/@Applicative@ instance.
newtype ZipFenceposted post a = ZipFenceposted { getZipFenceposted :: Fenceposted post a }
  deriving (Eq, Ord, Show, Read, Functor, F.Foldable, Traversable, Data, Typeable, Generic)

instance (Eq post) => Eq1 (ZipFenceposted post) where
  eq1 = (==)
  {-# INLINE eq1 #-}

instance (Ord post) => Ord1 (ZipFenceposted post) where
  compare1 = compare
  {-# INLINE compare1 #-}

instance (Read post) => Read1 (ZipFenceposted post) where
  readsPrec1 = readsPrec
  {-# INLINE readsPrec1 #-}

instance (Show post) => Show1 (ZipFenceposted post) where
  showsPrec1 = showsPrec
  {-# INLINE showsPrec1 #-}

instance Bitraversable1 ZipFenceposted where
  bitraverse1 f g = fmap ZipFenceposted . bitraverse1 f g . getZipFenceposted
  {-# INLINE bitraverse1 #-}

instance Bitraversable ZipFenceposted where
  bitraverse f g = fmap ZipFenceposted . bitraverse f g . getZipFenceposted
  {-# INLINE bitraverse #-}

instance Bifoldable ZipFenceposted where
  bifoldMap = bifoldMapDefault
  {-# INLINE bifoldMap #-}

instance Bifoldable1 ZipFenceposted where
  bifoldMap1 f g = bifoldMap1 f g . getZipFenceposted
  {-# INLINE bifoldMap1 #-}

instance Bifunctor ZipFenceposted where
  bimap = bimapDefault
  {-# INLINE bimap #-}

instance (Semigroup post) => Semigroup (ZipFenceposted post a) where
  ZipFenceposted a <> ZipFenceposted b = ZipFenceposted $ a Semi.<> b
  {-# INLINE (Semi.<>) #-}

instance (Monoid post) => Monoid (ZipFenceposted post a) where
  mempty = ZipFenceposted mempty
  {-# INLINE mempty #-}
  mappend (ZipFenceposted a) (ZipFenceposted b) = ZipFenceposted $ mappend a b
  {-# INLINE mappend #-}

-- | Zip together two @Fenceposted@s with the given combining functions.
fencepostZipWith :: (p -> q -> r) -> (a -> b -> c) -> Fenceposted p a -> Fenceposted q b -> Fenceposted r c
fencepostZipWith f g a b =
  case (projectFenceposted a, projectFenceposted b) of
    (FinalPost aPost, FinalPost bPost) -> fencepost (f aPost bPost)
    (FinalPost aPost, Panel bPost _ _) -> fencepost (f aPost bPost)
    (Panel aPost _ _, FinalPost bPost) -> fencepost (f aPost bPost)
    (Panel aPost aVal as, Panel bPost bVal bs) -> panel (f aPost bPost) (g aVal bVal) $ fencepostZipWith f g as bs
{-# INLINE fencepostZipWith #-}

fencepostRepeat :: (Monoid post) => a -> Fenceposted post a
fencepostRepeat x = panel mempty x $ fencepostRepeat x
{-# INLINE fencepostRepeat #-}

instance (Semigroup post) => Apply (ZipFenceposted post) where
  ZipFenceposted a <.> ZipFenceposted b = ZipFenceposted $ fencepostZipWith (Semi.<>) ($) a b
  {-# INLINE (<.>) #-}

instance (Monoid post) => Applicative (ZipFenceposted post) where
  pure = ZipFenceposted . fencepostRepeat
  {-# INLINE pure #-}
  a <*> b = first unwrapMonoid $ first WrapMonoid a <.> first WrapMonoid b
  {-# INLINE (<*>) #-}
