{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures, ConstraintKinds, PolyKinds #-}
{-# LANGUAGE GADTs, TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Data.Typeable
import Control.Monad
import Control.Monad.Free
-- import Control.Applicative
import qualified Data.Traversable as TR

-- import Data.Functor.Contravariant (Contravariant (..))
-- import Data.Bifunctor (Bifunctor (..))
-- import Data.Profunctor (Profunctor (..))

import Control.Category
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Control.Monad.Operational.Mini (ReifiedProgram (..))

import Data.Function (($), flip)
import Data.Tuple (fst)
import Data.Monoid (Endo (..))
import System.IO (IO)

import Prelude (seq, undefined)


import Control.Category.Natural
import Control.Category.Natural.Sum
-- import Relation.Binary.Classes
-- import Relation.Binary.Equational



 -- NOTE: 最終的な状態は捨てるしかない
variableN :: Monad m => s -> Natural (StateT s m) m
variableN s = Natural $ flip runStateT s >=> return . fst




-------------------------------------------------------------------------
-- * Objective からの写経

infixl 3 @-
newtype Obj (f :: * -> *) (g :: * -> *) = Obj { (@-) :: forall a. f a -> g (a, Obj f g) }
                                        deriving (Typeable)
-- type Obj f g = (Functor f, Functor g) => Obj' f g

instance HProfunctor Obj where
  hlmap f (Obj g) = Obj $ fmap (fmap (hlmap f)) . g . f
  {-# INLINE hlmap #-}
  hrmap f (Obj g) = Obj $ fmap (fmap (hrmap f)) . f . g
  {-# INLINE hrmap #-}
instance Functor f => HFunctor (Obj f) where
  hfmap = hrmap
  {-# INLINE hfmap #-}


-- 恒等射 echo は自明なオブジェクトである
echo :: Functor f => Obj f f
echo = Obj $ fmap (, echo)
{-# INLINE echo #-}




{- Functor制約が邪魔となって、これを定義できない
instance Category Obj where
  id = echo
  (Obj f) . (Obj g) = Obj $ fmap (\((x, f'), g') -> (x, f' . g')) . f . g
-}


fromNatural :: Functor g => Natural f g -> Obj f g
fromNatural (Natural f) = liftO f
{-# INLINE fromNatural #-}

liftO :: Functor g => (forall a. f a -> g a) -> Obj f g
liftO f = go where go = Obj $ fmap (,go) . f
{-# INLINE liftO #-}


infixr 1 @>>@
(@>>@) :: Functor h => Obj f g -> Obj g h -> Obj f h
(Obj m) @>>@ (Obj n) = Obj $ fmap (\((x, m'), n') -> (x, m' @>>@ n')) . n . m
{-# INLINE (@>>@) #-}

infixl 1 @<<@
(@<<@) :: (Monad h, Functor h) => Obj g h -> Obj f g -> Obj f h
(@<<@) = flip (@>>@)
{-# INLINE (@<<@) #-}

joinO :: Functor h => ((a, Obj f g), Obj g h) -> (a, Obj f h)
joinO ((x, a), b) = (x, a @>>@ b)
{-# INLINE joinO #-}


unfoldO :: Functor g => (forall a. r -> f a -> g (a, r)) -> r -> Obj f g
unfoldO f = go where go r = Obj $ fmap (fmap go) . f r
{-# INLINE unfoldO #-}

unfoldOM :: Monad m => (forall a. r -> f a -> m (a, r)) -> r -> Obj f m
unfoldOM f = go where go r = Obj $ liftM (fmap go) . f r
{-# INLINE unfoldOM #-}


statefull :: Monad m => (forall a. f a -> StateT s m a) -> s -> Obj f m
statefull f = go where go s = Obj $ \x -> runStateT (f x) s >>= \(a, s') -> s' `seq` return (a, go s')
{-# INLINE statefull #-}


iterObject :: Monad m => Obj f m -> Free f a -> m (a, Obj f m)
iterObject o (Pure a) = return (a, o)
iterObject o (Free f) = o @- f >>= \(cont, o') -> iterObject o' cont


iterative :: Monad m => Obj f m -> Obj (Free f) m
iterative = unfoldOM iterObject


variable :: Monad m => s -> Obj (StateT s m) m
variable s = Obj $ \m -> runStateT m s >>= return . fmap variable


announce :: (TR.Traversable t, Monad m) => f a -> StateT (t (Obj f m)) m [a]
announce f = StateT $ \t -> do
  (t', Endo e) <- runWriterT $ TR.mapM (\o -> lift (o @- f) >>= \(x, o') -> writer (o', Endo (x:))) t
  return (e [], t')

infixr 1 ^>>@
infixr 1 @>>^
(^>>@) :: Functor x => (forall a. h a -> g a) -> Obj g x -> Obj h x
(^>>@) = hlmap
(@>>^) :: (Functor g, Functor h) => Obj x h -> (forall a. h a -> g a) -> Obj x g
(@>>^) x f = hrmap f x

-------------------------------------------------------------------------
-- * 継承機能

-- | インターフェースの合成
extend :: Functor h => Obj f h -> Obj g h -> Obj (f ⨄ g) h
extend x y = Obj $ \case
  Intl z -> fmap (fmap (\x' -> extend x' y)) (x @- z)
  Intr z -> fmap (fmap (\y' -> extend x y')) (y @- z)

-- | メンバ変数の拡張
vextend :: (Functor m, Monad m) => t -> Obj (StateT s m) m -> Obj ((StateT t m) ⨄ (StateT s m)) m
vextend t = extend $ variable t

-- | インターフェースと実装の拡張
inherit :: Functor g => Obj (h ⨄ f) (f ⨄ g) -> Obj f g -> Obj (h ⨄ f) g
inherit x y = Obj $ \z -> elimSum (fmap (\((a, x'), y') -> (a, inherit x' y')) . (y @-) )
                          (fmap $ fmap (\x' -> inherit x' y)) (x @- z)



-------------------------------------------------------------------------

main :: IO ()
main = return ()


