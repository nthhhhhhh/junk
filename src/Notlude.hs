{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Notlude.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}


-- The Notlude is not not the bear necessities


module Notlude (
  module Exports,
) where

import Data.Eq            as Exports
import Data.Ord           as Exports
import Data.Int           as Exports
import Data.Bool          as Exports
import Data.Tuple         as Exports
import Data.Maybe         as Exports
import Data.Monoid        as Exports
import Data.Functor       as Exports
import Data.Foldable      as Exports
import Data.Function      as Exports
import Data.Traversable   as Exports

import Control.Applicative as Exports
import System.IO as Exports

import Control.Monad as Exports 
    ( (>>=)
    , return
    , join
    , liftM
    )

import Data.Sequence as Exports
    hiding 
    ( null
    , length
    , empty
    )

import Prelude as Exports
    ( (+)
    , (-)
    , (*)
    )
