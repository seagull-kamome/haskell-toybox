{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds, KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
-------------------------------------------------------------------------
--
module Data.FFI.String where

import Control.Applicative (Applicative, pure, (<$>))
import Control.Impossible
import GHC.TypeLits
import Data.Proxy
import qualified Data.HashTable.ST.Basic as HT
import Control.Monad.ST.Safe
-- import Data.Maybe (maybe)

newtype FFI (n :: Nat) t a = FFI {
  runFFI :: forall r. Monad t =>
            (KnownNat n => String -> Proxy n -> [t String] -> r)
            -> (t String -> r)
            -> r
  }

ffi :: (KnownNat n, Applicative t) => String -> Proxy n -> FFI n t a
ffi fmt arity = if natVal arity == 0
                then FFI $ \_ f -> f (pure fmt)
                else FFI $ \f _ -> f fmt arity []
{-# INLINE ffi #-}

ffiVal :: Monad t => t String -> FFI 0 t a
ffiVal x = FFI $ \_ f -> f x
{-# INLINE ffiVal #-}

ffiApply :: (KnownNat n, Monad t) => FFI (n + 1) t (a -> b) -> FFI 0 t a -> FFI n t b
ffiApply (FFI f) (FFI g) =
  FFI $ \h p ->
         g impossible $ \x ->
                         f (\fmt aty args ->
                             if natVal aty == 1
                             then p $ renderFFI fmt (args ++ [x])
                             else h fmt Proxy (args ++ [x]) ) impossible



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

newtype Q a = Q ( forall s. HT.HashTable s String Int -> ST s a )

runQ :: Q a -> ST s a
runQ (Q f) = HT.new >>= f

newName :: String -> Q String
newName prefix = Q $ \ht -> do
  n <- maybe 0 (+ 1) <$> HT.lookup ht prefix
  HT.insert ht prefix n
  return $ prefix ++ show n



----------------------------------------------------------------------------


