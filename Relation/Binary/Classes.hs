{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
-------------------------------------------------------------------------
--
module Relation.Binary.Classes (
  HasReflexitivity (..),
  HasSymmetry (..),
  HasTransitivity (..),
  Equality (..),
  REL (..), Rel
  ) where


-------------------------------------------------------------------------

newtype REL a b = REL { unREL :: forall op. op a b }
type Rel a = REL a a

-------------------------------------------------------------------------

class HasReflexitivity (op :: k -> k -> *) where refl :: op a a
class HasSymmetry (op :: k -> k -> *) where symmetry :: op a b -> op b a
class HasTransitivity (op :: k -> k -> *) where
  transitivity :: op a b -> op b c -> op a c

class (HasReflexitivity op, HasSymmetry op, HasTransitivity op) => Equality op where
  type EqualityCongConstraint (a :: * -> * -> *) (b :: * -> *) :: constraint
  type EqualityCongConstraint a b = ()
  type EqualityCong2Constraint (a :: * -> * -> *) (b :: * -> * -> *) :: constraint
  type EqualityCong2Constraint a b = ()
  castWith :: op a b -> a -> b
  subst ::EqualityCongConstraint op f => op a b -> f a -> f b
  subst2 :: EqualityCong2Constraint op f => op a b -> op c d -> f a c -> f b d
  cong :: EqualityCongConstraint op f => op a b -> op (f a) (f b)
  cong2 :: EqualityCong2Constraint op f => op a b -> op c d -> op (f a c) (f b d)

-------------------------------------------------------------------------

