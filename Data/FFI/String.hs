{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-------------------------------------------------------------------------
--
module Data.FFI.String where

import GHC.TypeLits
import Data.Proxy


newtype FFI (n :: Nat) t a = FFI {
  runFFI :: forall r. Monad t =>
            (KnownNat n => String -> Proxy n -> [t String] -> r)
            -> (t String -> r)
            -> r
  }
--            -> (forall b. FFI t (a -> b) -> t String -> r)-> r }

ffiFun :: (KnownNat n, Monad t) => String -> Proxy n -> FFI n t a
ffiFun fmt arity = FFI $ \f _ -> f fmt arity []

ffiVal :: Monad t => t String -> FFI 0 t a
ffiVal x = FFI $ \_ f -> f x

ffiApply :: (KnownNat n, Monad t) => FFI (n + 1) t (a -> b) -> FFI 0 t a -> FFI n t b
ffiApply (FFI f) (FFI g) =
  FFI $ \h p ->
         g (error "") $ \x ->
                         f (\fmt aty args ->
                             if natVal aty == 1
                             then p $ renderFFI fmt (args ++ [x])
                             else h fmt Proxy (args ++ [x]) ) (error "")


renderFFI :: Monad t => String -> [t String] -> t String
renderFFI fmt args = sequence args >>= return . go "" fmt where
  go xs [] _ = xs
  go xs ('$':n':ys) zs
    | n' >= '0' && n' <= '9' =
        let n = fromEnum n' - fromEnum '0'
        in go (xs ++ zs !! n) ys zs
    | otherwise = go (xs ++ ['$',n']) (ys) zs
  go xs (c:ys) zs = go (xs ++ [c]) ys zs





----------------------------------------------------------------------------



