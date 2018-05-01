{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Preamble
-- Copyright    : (C) 2014-2015 Dom De Re
-- License      : BSD3
-- Maintainer   : Dom De Re
--
-- The bits of the Prelude used in this project.
--
-------------------------------------------------------------------
module Preamble (
  -- * Type Classes
    Functor(..)
  , Foldable(..)
  , Traversable(..)
  , Alternative(..)
  , Applicative(..)
  , Bifunctor(..)
  , Monad(..)
  , MonadIO(..)
  , MonadPlus(..)
  , MonadReader(..)
  , MonadState(..)
  , MonadTrans(..)
  , Monoid(..)
  , Semigroup(..)
  , Show(..)
  , Eq(..)
  , Fractional(..)
  , Integral(..)
  , Num(..)
  , Ord(..)
  , RealFrac(..)
  -- * Types
  , Bool(..)
  , Char
  , Double
  , Int
  , Int8
  , Int16
  , Int32
  , Int64
  , Integer
  , Const(..)
  , Ordering(..)
  , NonEmpty(..)
  , String
  , Sum(..)
  , Product(..)
  , Void
  , Word8
  , Word16
  , Word32
  , Word64
  -- ** Natural Numbers
  , N.Natural
  , N.View
  -- ** Monads and Monad Transformers
  , Either(..)
  , ExceptT(..)
  , IO
  , Identity(..)
  , Maybe(..)
  , MaybeT(..)
  , Reader
  , ReaderT(..)
  , State
  , StateT(..)
  -- * Operators
  , (.)
  , ($)
  , ($!)
  , (<$>)
  , (++)
  , (&&)
  , (||)
  -- ** Monad
  , (=<<)
  , (>=>)
  , (<=<)
  -- * Functions
  , id
  , const
  , flip
  , fromIntegral
  , not
  , on
  , otherwise
  , ord
  -- ** Natural Numbers
  , foldNatural
  , N.monus
  , viewNatural
  -- ** Foldable
  , for_
  , forM_
  , mapM_
  , sequenceA_
  , traverse_
  -- ** Traversable
  , for
  , forM
  -- ** Tuple
  , curry
  , fst
  , snd
  , uncurry
  -- ** Either
  , either
  , lefts
  , rights
  , partitionEithers
  -- ** Maybe
  , catMaybes
  , fromMaybe
  , maybe
  -- ** Lists
  , chop
  , chunksOf
  , drop
  , dropWhile
  , take
  , takeWhile
  , filter
  , notElem
  , partition
  , zip
  , zipWith
  -- ** NonEmpty Lists
  , group
  , groupBy
  , group1
  , groupBy1
  , nonEmpty
  , head
  , init
  , last
  , tail
  -- ** Functor
  , void
  -- ** Applicative
  , optional
  -- ** Monad
  , join
  , replicateM
  , replicateM_
  , runReader
  , runState
  , unless
  , when
  -- ** Monad Transformers
  , asks
  , gets
  -- *** ExceptT
  , catchE
  , mapExceptT
  , runExceptT
  , throwE
  , withExceptT
  -- * Void
  , absurd
  ) where

import Prelude (
    Show(..)
  , Eq(..)
  , Fractional(recip, fromRational)
  , Integral(..)
  , Num(..)
  , RealFrac(..)
  , Double
  , Integer
  , Ordering(..)
  , ($)
  , ($!)
  , (.)
  , fromIntegral
  , otherwise
  )
import Control.Applicative
import Control.Monad (Monad(..), MonadPlus(..), (=<<), (>=>), (<=<), join, replicateM, replicateM_, unless, when)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader(..), Reader, ReaderT(..), asks, runReader)
import Control.Monad.State (MonadState(..), State, StateT(..), gets, runState)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Maybe (MaybeT(..))
-- EitherT was good enough, but it looks like the rest of the world will be using ExceptT so now i have to make the aesthetic
-- change and use the crappier functions
import Control.Monad.Trans.Except (ExceptT(..), catchE, mapExceptT, runExceptT, throwE, withExceptT)
import Data.Bifunctor (Bifunctor(..))
import Data.Bool (Bool(..), (&&), (||), not)
import Data.Char (Char, ord)
import Data.Either (Either(..), either, lefts, rights, partitionEithers)
import Data.Foldable (Foldable(..), for_, forM_, sequenceA_, traverse_, notElem, mapM_, toList)
import Data.Function (const, flip, id, on)
import Data.Functor (Functor(..), void)
import Data.Int (Int, Int8, Int16, Int32, Int64)
import Data.List ((++), drop, dropWhile, filter, partition, take, takeWhile, zip, zipWith)
import Data.List.NonEmpty (NonEmpty(..), group, groupBy, group1, groupBy1, init, last, nonEmpty, head, tail)
import Data.List.Split (chop, chunksOf)
import Data.Maybe (Maybe(..), catMaybes, fromMaybe, maybe)
import Data.Monoid (Monoid(..), Sum(..), Product(..))
import qualified Data.Natural as N (Natural, View, fold, monus, view)
import Data.Ord (Ord(..))
import Data.Semigroup (Semigroup(..))
import Data.String (String)
import Data.Traversable (Traversable(..), for, forM, mapM)
import Data.Tuple (fst, snd, curry, uncurry)
import Data.Void (Void, absurd)
import Data.Word (Word8, Word16, Word32, Word64)
import System.IO (IO)

foldNatural :: a -> (a ->a) -> N.Natural -> a
foldNatural = N.fold

viewNatural :: N.Natural -> N.View
viewNatural = N.view
