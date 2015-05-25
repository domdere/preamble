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
    ,   Foldable(..)
    ,   Traversable(..)
    ,   Alternative(..)
    ,   Applicative(..)
    ,   Bifunctor(..)
    ,   Monad(..)
    ,   MonadReader(..)
    ,   MonadState(..)
    ,   Monoid(..)
    ,   Semigroup(..)
    ,   Show(..)
    ,   Eq(..)
    ,   Num(..)
    ,   Ord(..)
    -- * Types
    ,   Bool(..)
    ,   Char
    ,   Double
    ,   Int
    ,   NonEmpty(..)
    ,   String
    ,   Word16
    -- ** Monads and Monad Transformers
    ,   Either(..)
    ,   EitherT(..)
    ,   IO
    ,   Identity(..)
    ,   Maybe
    ,   Reader
    ,   ReaderT(..)
    ,   State
    ,   StateT(..)
    -- * Operators
    ,   (.)
    ,   ($)
    ,   (<$>)
    ,   (++)
    ,   (&&)
    ,   (||)
    -- * Functions
    ,   id
    ,   const
    ,   flip
    ,   either
    ,   fromIntegral
    ,   maybe
    ,   on
    ,   otherwise
    ,   ord
    ,   unless
    ,   when
    -- ** Foldable
    ,   sequenceA_
    ,   traverse_
    -- ** NonEmpty Lists
    ,   nonEmpty
    ,   head
    ,   tail
    -- ** Monad
    ,   replicateM
    ,   replicateM_
    -- ** Monad Transformers
    ,   lift
    ,   asks
    ,   gets
    -- *** EitherT
    ,   bimapEitherT
    ,   hoistEither
    ,   left
    ,   mapEitherT
    ) where

import Prelude hiding ( head, tail )
import Control.Applicative
import Control.Monad ( replicateM, replicateM_, unless, when )
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Reader ( MonadReader(..), Reader, ReaderT(..), asks )
import Control.Monad.State ( MonadState(..), State, StateT(..), gets )
import Control.Monad.Trans ( lift )
import Control.Monad.Trans.Either ( EitherT(..), bimapEitherT, hoistEither, left, mapEitherT )
import Data.Bifunctor ( Bifunctor(..) )
import Data.Char ( ord )
import Data.Foldable ( Foldable(..), sequenceA_, traverse_ )
import Data.Function ( on )
import Data.List.NonEmpty ( NonEmpty(..), nonEmpty, head, tail)
import Data.Monoid ( Monoid(..) )
import Data.Semigroup ( Semigroup(..) )
import Data.Traversable ( Traversable(..) )
import Data.Word ( Word16 )
