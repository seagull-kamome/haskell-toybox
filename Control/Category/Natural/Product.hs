-------------------------------------------------------------------------
-- * 直積
{-# LANGUAGE KindSignatures, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Control.Category.Natural.Product (
  (×) (..),
   elimProd,
   prod_commutative,
   prod_associative
  ) where

import Relation.Binary.Equational

data (×) :: (* -> *) -> (* -> *) -> (* -> *) where
  Prod :: { proj₁ :: f a, proj₂ :: g a } -> (f × g) a

elimProd :: (f a -> g a -> r) -> (f × g) a -> r
elimProd f (Prod x y) = f x y
{-# INLINE elimProd #-}

prod_commutative :: (f × g) a -> (g × f) a
prod_commutative = elimProd (flip Prod)
{-# INLINE prod_commutative #-}

prod_associative :: ((f × g) × h) a ⇔ (f × (g × h)) a
prod_associative = MkIso fromto tofrom where
  fromto = elimProd (\xy z -> elimProd (\x y -> Prod x $ Prod y z) xy)
  tofrom = elimProd (\x -> elimProd (\y z -> Prod (Prod x y) z))

