{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures, PolyKinds, ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE OverloadedLists #-}
module Data.List.Strange where

import GHC.TypeLits
import GHC.Exts (Constraint, IsList (..))
-- import Data.Void

----------------------------------------------------------------------------

data NList :: Nat -> * -> * where
  NNil :: NList 0 a
  NCons :: a -> NList n a -> NList (n + 1) a


-- nが変化すると型が合わなくなるので，IsListは定義できない
-- instance IsList (NList n a) where
--   type Item (NList n a) = a
--   fromList [] = NNil
--   fromList (x:xs) = NCons x (fromList xs)
--   toList NNil = []
--   toList (NCons x xs) = x : toList xs

data HList :: [*] -> * where
  HNil :: HList '[]
  HCons :: a -> HList xs-> HList (a ': xs)

data NHList :: Nat -> [*] -> * where
  NHNil :: NHList 0 '[]
  NHCons :: a -> NHList n xs -> NHList (n + 1) (a ': xs)


data CList :: (* -> Constraint) -> * where
  CNil :: CList c
  CCons :: (forall a. c a => a) -> CList c -> CList c

data NCList :: Nat -> (* -> Constraint) -> * where
  NCNil :: NCList 0 c
  NCCons :: (c a => a) -> NCList n c -> NCList (n + 1) c

----------------------------------------------------------------------------


data IList :: (k -> *) -> k -> k -> * where
  INil :: IList f i i
  ICons :: f j -> IList f l i -> IList f i j


{-
type family Id (x :: k) :: k
type instance Id x = x

type family Const (x :: k) (y :: l) :: k
type instance Const x y = x

type family Fst (a :: (k, l)) :: k
type instance Fst '(x, y) = x

type family Snd (a :: (k, l)) :: l
type instance Snd '(x, y) = y

type family NList' (n :: Nat) a :: *
type instance NList' 0 a = IList (Const a) 0 0
type instance NList' n a = forall m. ((m + 1) ~ n) => IList (Const a) m n

-}

