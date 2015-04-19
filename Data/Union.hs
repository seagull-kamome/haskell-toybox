{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds, KindSignatures,GADTs,
             TypeOperators,
             FlexibleInstances,
             ScopedTypeVariables #-}
module Data.Union (
  Union (..),
  inject, project, wrap, unwrap,
  push, pop,
  swap, rotate,
  conceal
  ) where

import Data.Type.Equality ((:~:) (..), gcastWith, testEquality)

import Data.Type.List hiding (length)
import Data.Type.List.Index (Index (..))
import qualified Data.Type.List.Index as Index
import Algebra.Absurd

----------------------------------------------------------------------------

data Union (l :: [k]) where
  Union :: Index l e -> e -> Union l

instance Absurd (Union '[]) where
  absurd (Union x _) = absurd x

inject :: Member e l => e -> Union l
inject = Union Index.index

project :: forall l e. Member e l => Union l -> Maybe e
project (Union i x) = fmap (\refl -> gcastWith refl x) $ testEquality i (Index.index :: Index l e)

wrap :: e -> Union (e ': l)
wrap = inject

unwrap :: Union '[e] -> e
unwrap (Union i x) = gcastWith (Index.trivial i) x

----------------------------------------------------------------------------

push :: Union l -> Union (e ': l)
push (Union i x) = Union (Index.push i) x

pop :: Union (e ': l) -> Either e (Union l)
pop (Union i x) = case Index.pop i of
                   Left Refl -> Left x
                   Right i' -> Right $ Union i' x


swap :: Union (e ': f ': l) -> Union (f ': e ': l)
swap (Union i x) = Union (Index.swap i) x

rotate :: Union (e ': f ': g ': l) -> Union (f ': g ': e ': l)
rotate (Union i x) = Union (Index.rotate i) x

----------------------------------------------------------------------------

conceal :: Member e l => Union (e ': l) -> Union l
conceal (Union i x) = Union (Index.conceal i) x

