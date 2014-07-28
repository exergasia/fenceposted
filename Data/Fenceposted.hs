{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric #-}
module Data.Fenceposted
  ( Fenceposted(..)
  , fencepost
  , finalPostL
  , postValuePairsL
  , FencepostedF(..)
  , fencepostedF
  , tritraverseFencepostedF
  , embed
  , project
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

instance (Ord post) => Ord1 (Fenceposted post) where
  compare1 = compare

instance (Read post) => Read1 (Fenceposted post) where
  readsPrec1 = readsPrec

instance (Show post) => Show1 (Fenceposted post) where
  showsPrec1 = showsPrec

-- | @'finalPostL' :: Lens\' ('Fenceposted' post a) post@
finalPostL :: (Functor f) => (post -> f post) -> Fenceposted post a -> f (Fenceposted post a)
finalPostL f (Fenceposted xs z) = Fenceposted xs <$> f z

-- | @'postValuePairsL' :: Lens ('Fenceposted' post a) ('Fenceposted' post a\') [(post, a)] [(post, a\')]@
postValuePairsL :: (Functor f) => ([(post, a)] -> f [(post, a')]) -> Fenceposted post a -> f (Fenceposted post a')
postValuePairsL f (Fenceposted xs z) = flip Fenceposted z <$> f xs

fencepost :: post -> Fenceposted post a
fencepost = Fenceposted mempty

instance Bitraversable Fenceposted where
  bitraverse f g (Fenceposted xs z) = Fenceposted <$> traverse (bitraverse f g) xs <*> f z

instance Bifoldable Fenceposted where
  bifoldMap = bifoldMapDefault

instance Bifoldable1 Fenceposted where
  bifoldMap1 f g (Fenceposted xs z) = F.foldr (Semi.<>) (f z) $ bifoldMap1 f g <$> xs

instance Bifunctor Fenceposted where
  bimap = bimapDefault

instance (Semigroup post) => Semigroup (Fenceposted post a) where
  Fenceposted as aEnd <> b =
    case project b of
      Panel bStart x (Fenceposted rest bEnd) -> Fenceposted (as <> pure (aEnd Semi.<> bStart, x) <> rest) bEnd
      FinalPost bEnd -> Fenceposted as (aEnd Semi.<> bEnd)

instance (Monoid post) => Monoid (Fenceposted post a) where
  mempty = fencepost mempty
  mappend a b = first unwrapMonoid $ first WrapMonoid a Semi.<> first WrapMonoid b

-- | A \'productish\' instance.
instance (Semigroup post) => Apply (Fenceposted post) where
  f <.> a = Bind.join $ (<$> a) <$> f

instance (Monoid post) => Applicative (Fenceposted post) where
  pure x = Fenceposted (pure (mempty, x)) mempty
  f <*> a = first unwrapMonoid $ first WrapMonoid f <.> first WrapMonoid a

instance (Semigroup post) => Bind.Bind (Fenceposted post) where
  join = bifoldMap1 fencepost id

instance (Monoid post) => Monad (Fenceposted post) where
  return = pure
  a >>= f = first unwrapMonoid $ Bind.join $ fmap (first WrapMonoid . f) (first WrapMonoid a)

instance (Semigroup post) => Alt (Fenceposted post) where
  (<!>) = (Semi.<>)

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
  deriving (Eq, Ord, Show, Read, Traversable, F.Foldable, Functor, Data, Typeable, Generic)

instance (Eq post, Eq a) => Eq1 (FencepostedF post a) where
  eq1 = (==)

instance (Ord post, Ord a) => Ord1 (FencepostedF post a) where
  compare1 = compare

instance (Read post, Read a) => Read1 (FencepostedF post a) where
  readsPrec1 = readsPrec

instance (Show post, Show a) => Show1 (FencepostedF post a) where
  showsPrec1 = showsPrec

fencepostedF :: (post -> x) -> (post -> a -> r -> x) -> FencepostedF post a r -> x
fencepostedF f _ (FinalPost post) = f post
fencepostedF _ g (Panel post a r) = g post a r

tritraverseFencepostedF :: (Applicative f) => (post -> f post') -> (a -> f a') -> (r -> f r') -> FencepostedF post a r -> f (FencepostedF post' a' r')
tritraverseFencepostedF f g h = fencepostedF (fmap FinalPost . f) (\ post a r -> Panel <$> f post <*> g a <*> h r)

embed :: FencepostedF post a (Fenceposted post a) -> Fenceposted post a
embed = fencepostedF fencepost (\ post x (Fenceposted xs z) -> Fenceposted ((post, x) : xs) z)

project :: Fenceposted post a -> FencepostedF post a (Fenceposted post a)
project (Fenceposted xs z) =
  case xs of
    (post, x) : rest -> Panel post x (Fenceposted rest z)
    [] -> FinalPost z
