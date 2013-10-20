-- | A better Monoid instance for Data.Map (at least for my uses)
module MonoidMap where

import Data.Map (Map, empty, unionWith, singleton)
import Data.Semigroup

newtype MonoidMap k v = MMap { getMap :: Map k v }

instance (Ord k, Semigroup v) => Semigroup (MonoidMap k v) where
  (MMap m1) <> (MMap m2) = MMap $ unionWith (<>) m1 m2

instance (Ord k, Semigroup v) => Monoid (MonoidMap k v) where
  mempty = MMap empty
  mappend = (<>)

singletonMMap :: k -> v -> MonoidMap k v
singletonMMap k v = MMap $ singleton k v
