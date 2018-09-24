{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  InfixFunctor.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module InfixFunctor where

import Notlude


infixl 4 <$$>
(<$$>) :: (Functor f1, Functor f2)
       => (a -> b) -> f1 (f2 a) -> f1 (f2 b)
(<$$>) f x =
  fmap (fmap f) x



infixl 1 <&&>
(<&&>) :: (Functor f1, Functor f2) 
       => f1 (f2 a) -> (a -> b) -> f1 (f2 b)
(<&&>) x f =
  fmap (fmap f) x



infixl 1 <&&&>
(<&&&>) :: (Functor f1, Functor f2, Functor f3) 
        => f1 (f2 (f3 a)) -> (a -> b) -> f1 (f2 (f3 b))
(<&&&>) x f =
  fmap (fmap (fmap f)) x

