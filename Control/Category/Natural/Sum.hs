-------------------------------------------------------------------------
-- * 直和
{-# LANGUAGE KindSignatures, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Natural.Sum (
  (⨄) (..),
  elimSum,
  eraseSum,
  sum_commutative,
  sum_associative
  ) where

import Relation.Binary.Equational

-------------------------------------------------------------------------

data (⨄) :: (* -> *) -> (* -> *) -> (* -> *) where
  Intl :: f a -> (f ⨄ g) a
  Intr :: g a -> (f ⨄ g) a


elimSum :: (f a -> r) -> (g a -> r) -> (f ⨄ g) a -> r
elimSum h _ (Intl x) = h x
elimSum _ h (Intr x) = h x
{-# INLINE elimSum #-}

eraseSum :: (f ⨄ f) a -> f a
eraseSum = elimSum id id
{-# INLINE eraseSum #-}

sum_commutative :: (f ⨄ g) a -> (g ⨄ f) a
sum_commutative = elimSum Intr Intl
{-# INLINE sum_commutative #-}


sum_associative :: ((f ⨄ g) ⨄ h) a ⇔ (f ⨄ (g ⨄ h)) a
sum_associative = MkIso fromto tofrom where
  fromto = elimSum (elimSum Intl (Intr . Intl)) (Intr . Intr)
  tofrom = elimSum (Intl . Intl) (elimSum (Intl . Intr) Intr)


