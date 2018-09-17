{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  ParseMeta.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module ParseMeta where

import Notlude

import Data.Text
import Data.Sequence as S
import Data.List as L (head, last)



isjnkstr :: Text -> Bool
isjnkstr t = 
  let
    t'         = strip t
    hasleftc   = isInfixOf  "^" t'
    hasrightc  = isPrefixOf "^" t'
    nonempty   = not $ Data.Text.null t'
    equalinfix = isInfixOf  "=" t'
  in
    nonempty 
    && hasleftc
    && hasrightc
    && equalinfix



parsejnkstr :: Text -> (Text, Text)
parsejnkstr t =
  (\xs -> (L.head xs, L.last xs))
  $ splitOn "="
  . flip S.index 1
  . fromList $ splitOn "^" (strip t)

