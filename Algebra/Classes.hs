-------------------------------------------------------------------------
--
{-# LANGUAGE KindSignatures, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
module Algebra.Classes (
  LeftAssociative, RightAssociative, Associative,
  HasAssociativity (..),
  --
  Commutative,
  HasCommutative (..)
  ) where

import Relation.Binary.Equational

-------------------------------------------------------------------------
type Commutative op a b c = op a b -> op b c

class HasCommutative op where
  commutative :: Commutative op a b c


-------------------------------------------------------------------------

type LeftAssociative op a b c = op (op a b) c -> op a (op b c)
type RightAssociative op a b c = op a (op b c) -> op (op a b) c
type Associative op a b c = (LeftAssociative op a b c) ⇔ (RightAssociative op a b c)

class HasAssociativity (op :: * -> * -> *) where
  left_associative :: LeftAssociative op a b c -- op (op a b) c -> op a (op b c)
  left_associative = fromTo associative

  right_associative :: RightAssociative op a b c -- op a (op b c) -> op (op a b) c
  right_associative = toFrom associative

  associative :: op (op a b) c ⇔ op a (op b c)
  associative = MkIso left_associative right_associative

-------------------------------------------------------------------------

