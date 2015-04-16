{-# LANGUAGE RankNTypes,
             DataKinds, PolyKinds,
             ConstraintKinds,
             TypeFamilies, TypeOperators,
             ScopedTypeVariables,
             FlexibleInstances, UndecidableInstances,
             FlexibleContexts #-}
module Data.Type.List (
  (:++),
  Head, Tail, Drop, Take,
  Elem, Remove,
  Length, IndexOf, Member, Is, InstanceOf,
  Lookup, Filter,
  Data.Type.List.length
  ) where


-- import GHC.Exts (Constraint)
import GHC.TypeLits
import Data.Proxy
import Data.Type.Bool

----------------------------------------------------------------------------


type family l :++ r where
  '[] :++ r = r
  (x ': l) :++ r = (x ': (l :++ r))


type family Length l where
  Length '[] = 0
  Length (e ': l) = 1 + Length l


type family IndexOf e l where
  IndexOf e (e ': l) = 0
  IndexOf e (f ': l) = 1 + IndexOf e l

-- メンバであるという事は、インデックスが定義できるという事である
type Member e l = KnownNat (IndexOf e l)


----------------------------------------------------------------------------

type family Is (x :: k) (y :: * -> *) :: Bool
type family InstanceOf e l where
  InstanceOf e (x ': l) = If (Is e x) x (InstanceOf e l)


----------------------------------------------------------------------------

type family Head l where
  Head (e ': l) = e

type family Tail l where
  Tail (e ': l) = l

type family Drop n l where
  Drop 0 l = l
  Drop n (x ': l) = Drop (n - 1) l

type family Take n l where
  Take 0 l = '[]
  Take n (x ': l) = (x ': Take (n - 1) l)

----------------------------------------------------------------------------

type family Elem e l where
  Elem e '[] = False
  Elem e (e ': l) = True
  Elem e (f ': l) = Elem e l

type family Remove e l where
  Remove e (e ': l) = l
  Remove e (f ': l) = (f ': Remove e l)

----------------------------------------------------------------------------

type family Lookup e l where
  Lookup e ('(e , x) ': l) = x
  Lookup e (x ': l) = Lookup e l

type family Filter p l where
  Filter p '[] = '[]
  Filter p (x ': l) = If (p x) (x ': Filter p l) (Filter p l)

----------------------------------------------------------------------------

length :: forall l proxy. KnownNat (Length l) => proxy l -> Integer
length _ = natVal (Proxy :: Proxy (Length l))





