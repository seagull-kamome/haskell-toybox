-------------------------------------------------------------------------
--
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Relation.Binary.Equational (
  (≡) (..), (⇔) (..)
  ) where

import Data.Function (($))
import Data.Functor (Functor (..))
import Data.Typeable
-- import Data.Profunctor (Profunctor (..))
import Data.Bifunctor (Bifunctor (..))
import Control.Category

-- import Control.Category.Natural
import Relation.Binary.Classes

-- import Prelude (Eq (..))

-------------------------------------------------------------------------


data (≡) :: k -> k -> * where
  EqRefl :: a ≡ a
  deriving (Typeable)

instance Category (≡) where
  id = EqRefl
  EqRefl . EqRefl = EqRefl
instance HasReflexitivity (≡) where refl = EqRefl
instance HasSymmetry (≡) where symmetry EqRefl = EqRefl
instance HasTransitivity (≡) where transitivity EqRefl EqRefl = EqRefl
instance Equality (≡) where
  castWith EqRefl = id
  subst EqRefl = id
  subst2 EqRefl EqRefl = id
  cong EqRefl = id
  cong2 EqRefl EqRefl = id


-------------------------------------------------------------------------


data (⇔) :: * -> * -> * where
  MkIso :: { fromTo :: a -> b , toFrom :: b -> a } -> a ⇔ b
  deriving (Typeable)

instance Category (⇔) where
  id = MkIso id id 
  (MkIso f g) . (MkIso f' g') = MkIso (f . f') (g' . g)
instance HasReflexitivity (⇔) where refl = id
instance HasSymmetry (⇔) where symmetry (MkIso f g) = MkIso g f
instance HasTransitivity (⇔) where
  transitivity (MkIso f g) (MkIso h i) = MkIso (h . f) (g . i)
instance Equality (⇔) where
  type EqualityCongConstraint (⇔) f = Functor f
  type EqualityCong2Constraint (⇔) f = Bifunctor f
  castWith x = fromTo x
  subst x = fmap $ fromTo x
  subst2 x y = bimap (fromTo x) (fromTo y)
  cong x = MkIso (subst x) (fmap $ toFrom x)
  cong2 x y = MkIso (subst2 x y) (bimap (toFrom x) (toFrom y))


-------------------------------------------------------------------------

{-
newtype Equation a b = Equation { unEquation :: (forall op. Category op, Equality op) => op a b }
                       deriving (Typeable)
instance Category Equation where
  id = Equation id
  (Equation x) . (Equation y) = Equation (x . y)

instance HasReflexitivity Equation where refl = Equation refl
instance HasSymmetry Equation where symmetry = Equation symmetry
instance HasTransitivity Equation where 
instance Equality Equation where
-}




