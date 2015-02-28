-------------------------------------------------------------------------
-- * 自然変換
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Control.Category.Natural (
  HFunctor (..), HContravariant (..),
  HBifunctor (..), HProfunctor (..),
  Natural (..)
  ) where

import Control.Category
import Data.Functor
import Data.Typeable

-------------------------------------------------------------------------

class HFunctor (f :: (* -> *) -> *) where
  hfmap :: (Functor g, Functor h) => (forall a. g a -> h a) -> f g -> f h
class HContravariant (f :: (* -> *) -> *) where
  hfcontramap :: (Functor g, Functor h) => (forall a. g a -> h a) -> f h -> f g
class HBifunctor (f :: (* -> *) -> (* -> *) -> *) where
  hfirst :: (Functor x) => (forall a. g a -> h a) -> f g x -> f h x
  hfirst g = hbimap g id
  hsecond :: (Functor g, Functor h) => (forall a. g a -> h a) -> f x g -> f x h
  hsecond g = hbimap id g
  hbimap :: (Functor i, Functor j) => (forall a. g a -> h a) -> (forall a. i a -> j a) -> f g i -> f h j
  hbimap f g = hsecond g . hfirst f
class HProfunctor (f :: (* -> *) -> (* -> *) -> *) where
  hlmap :: (Functor x) => (forall a. g a -> h a) -> f h x -> f g x
  hlmap g = hdimap g id
  hrmap :: (Functor g, Functor h) => (forall a. g a -> h a) -> f x g -> f x h
  hrmap g = hdimap id g
  hdimap :: (Functor i, Functor j) => (forall a. g a -> h a) -> (forall a. i a -> j a) -> f h i -> f g j
  hdimap f g = hlmap f . hrmap g

-- instance HBifunctor f => HFunctor (f x) where hfmap = hsecond
-- instance HProfunctor f => HFunctor (f x) where hfmap = hrmap

-------------------------------------------------------------------------

newtype Natural f g = Natural { runNatural :: forall a. f a -> g a } deriving (Typeable)

instance Category Natural where
  id = Natural id
  (Natural f) . (Natural g) = Natural (f . g)
instance HProfunctor Natural where
  hlmap f (Natural g) = Natural (g . f)
  hrmap f (Natural g) = Natural (f .g)
instance HFunctor (Natural x) where
  hfmap = hrmap



