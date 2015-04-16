{-# LANGUAGE RankNTypes,
             DataKinds, PolyKinds,
             ConstraintKinds,
             TypeFamilies, TypeOperators,
             ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances,
             FlexibleContexts #-}
module Data.Type.List.Index where

import GHC.TypeLits
import Data.Type.Equality ((:~:) (..), TestEquality (..))
import Data.Proxy
import Unsafe.Coerce (unsafeCoerce)

import Data.Type.List

----------------------------------------------------------------------------

newtype Index l e = Index Integer

instance TestEquality (Index l) where
  testEquality (Index i) (Index j)
    | i == j = Just (unsafeCoerce Refl)
    | otherwise = error "non-trivial Index"

zero :: Index (e ': l) e
zero = Index 0

index :: forall e l. Member e l => Index l e
index = Index $ natVal (Proxy :: Proxy (IndexOf e l))

absurd :: Index '[] e -> a
absurd (Index i) = i `seq` error "absurd Index"

trivial :: Index [e] f -> f :~: e
trivial (Index i)
  | i == 0 = unsafeCoerce Refl
  | otherwise = error "non-trivial Index"

----------------------------------------------------------------------------

push :: Index l e -> Index (f ': l) e
push (Index i) = Index $ i + 1

pop :: Index (f ': l) e -> Index l e
pop (Index i)
  | i == 0 = error "empty index"
  | otherwise = Index $ i - 1

swap :: Index (e ': f ': l) e -> Index (f ': e ': l) e
swap (Index i)
  | i == 0 = Index 1
  | i == 1 = Index 0
  | otherwise = Index i

rotate :: Index (e ': f ': g ': l) h -> Index (f ': g ': e ': l) h
rotate (Index i)
  | i == 0 = Index 2
  | i == 1 = Index 0
  | i == 2 = Index 1
  | otherwise = Index i

prepend :: KnownNat (Length l) => proxy l -> Index m e -> Index (l :++ m) e
prepend p (Index i) = Index $ i + Data.Type.List.length p


append :: Index l e -> proxy m -> Index (l :++ m) e
append (Index i) _ = Index i

split :: forall e l m. KnownNat (Length l) => Index (l :++ m) e -> Either (Index l e) (Index m e)
split (Index i)
  | i < n = Left $ Index i
  | otherwise = Right $ Index $ i - n
  where n = Data.Type.List.length (Proxy :: Proxy l)

----------------------------------------------------------------------------

conceal :: forall e f l. Member f l => Index (f ': l) e -> Index l e
conceal (Index i)
  | i == 0 = let (Index j) = (index :: Index l f) in Index j
  | otherwise = Index $ i - 1

reveal :: forall e f l. Member f l => Index l e -> Index (f ': l) e
reveal (Index i)
  | i == j = Index 0
  | otherwise = Index $ i + 1
  where Index j = index :: Index l f

----------------------------------------------------------------------------


