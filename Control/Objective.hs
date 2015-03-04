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
-------------------------------------------------------------------------
--

module Control.Objective (
  Obj (..),
  echo,
  fromNatural,
  liftO, joinO,
  (@>>@), (@<<@), (@>>^), (^>>@),
  unfoldO, unfoldOM,
  statefull,
  iterObject,
  sequential,
  iterative,
  variable,
  announce,
  --
  extend, vextend, inherit,
  upcast,
  ObjDom, ObjCod, Decorate,
  --
  ObjRef (..), new, (.-),
  invokeOn
  ) where


import Data.Typeable
import Control.Monad
import Control.Monad.Free
-- import Control.Applicative
import qualified Data.Traversable as TR

-- import Data.Functor.Contravariant (Contravariant (..))
-- import Data.Bifunctor (Bifunctor (..))
-- import Data.Profunctor (Profunctor (..))

import Control.Category
import Control.Category.Natural
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Writer.Strict
import Control.Monad.Trans.Class
import Control.Monad.IO.Class
import Control.Monad.Operational.Mini (ReifiedProgram (..))
import Control.Concurrent.STM.TMVar (TMVar, newTMVarIO, takeTMVar, putTMVar)
import Control.Concurrent.STM (atomically)

import Data.Function (($), flip)
--import Data.Tuple (fst)
import Data.Monoid (Endo (..))

import Prelude (seq)


-- import Control.Category.Natural
import Control.Category.Natural.Sum
-- import Relation.Binary.Classes
-- import Relation.Binary.Equational



-------------------------------------------------------------------------
-- * Objective からの写経


--
-- NOTE:
--   メッセージによってコンテキストが変わるケースの対処。
--  あるメッセージはIOを要求するが、別のメッセージは違う
--  コンテキストを使う場合、どのような形になるだろうか？
--

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


sequential :: Monad m => Obj f m -> Obj (ReifiedProgram f) m
sequential o = Obj $ liftM (fmap sequential) . eval o where
  eval o' (Return x) = return (x, o')
  eval o' (f :>>= c) = o' @- f >>= \(x, o'') -> eval o'' (c x)


{-
sequential :: Monad m => Obj f m -> Obj (Program f) m
sequential o = Obj $ \prog -> liftM (fmap sequential) . unProgram prog (return . (,o)) (bnd o) where
  bnd o' f c = (o' @- f) >>= \(x, o'') -> c x
-}


iterative :: Monad m => Obj f m -> Obj (Free f) m
iterative = unfoldOM iterObject
{-# INLINE iterative #-}

-- NOTE: 最終的な状態は捨てるしかない
-- variableN :: Monad m => s -> Natural (StateT s m) m
-- variableN s = Natural $ flip runStateT s >=> return . fst

variable :: Monad m => s -> Obj (StateT s m) m
variable s = Obj $ \m -> runStateT m s >>= return . fmap variable
{-# INLINE variable #-}

announce :: (TR.Traversable t, Monad m) => f a -> StateT (t (Obj f m)) m [a]
announce f = StateT $ \t -> do
  (t', Endo e) <- runWriterT $ TR.mapM (\o -> lift (o @- f) >>= \(x, o') -> writer (o', Endo (x:))) t
  return (e [], t')

infixr 1 ^>>@
infixr 1 @>>^
(^>>@) :: Functor x => (forall a. h a -> g a) -> Obj g x -> Obj h x
(^>>@) = hlmap
{-# INLINE (^>>@) #-}
(@>>^) :: (Functor g, Functor h) => Obj x h -> (forall a. h a -> g a) -> Obj x g
(@>>^) x f = hrmap f x
{-# INLINE (@>>^) #-}

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
{-# INLINE vextend #-}

-- | インターフェースと実装の拡張
inherit :: Functor g => Obj (h ⨄ f) (f ⨄ g) -> Obj f g -> Obj (h ⨄ f) g
inherit x y = Obj $ \z -> elimSum (fmap (\((a, x'), y') -> (a, inherit x' y')) . (y @-) )
                          (fmap $ fmap (\x' -> inherit x' y)) (x @- z)

-- | アップキャストは必然的にこうなる
-- 適切なレベルまで一気にキャストする方法が無いと厳しいかも
upcast :: Functor g => Obj (h ⨄ f) g -> Obj f g
upcast = hlmap Intr



type family ObjDom o :: * -> *
type instance ObjDom (Obj f g) = f

type family ObjCod o :: * -> *
type instance ObjCod (Obj f g) = g

-- ここまでやるなら ExtensibleEffectつかった方が良くね？
type Decorate base impl ext = Obj (impl ⨄ ObjDom base) (ext ⨄ ObjDom base ⨄ ObjCod base)


-------------------------------------------------------------------------
-- * 参照

newtype ObjRef f g = ObjRef { unObjRef :: TMVar (Obj f g) }
                   deriving (Typeable)

new :: MonadIO m => Obj f g -> m (ObjRef f g)
new = liftIO . fmap ObjRef . newTMVarIO

(.-) :: MonadIO m => ObjRef f m -> f a -> m a
(.-) = invokeOn id


invokeOn :: MonadIO m => (forall x. g x -> m x) -> ObjRef f g -> f a -> m a
invokeOn f (ObjRef ref) msg = do
  o <- liftIO $ atomically $ takeTMVar ref
  (r, o') <- f $ o @- msg
  liftIO $ atomically $ putTMVar ref o'
  return r



