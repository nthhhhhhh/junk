{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Archive.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module Archive (tags) where

import Notlude

import Data.Text (Text)
import Text.HTML.TagSoup (Tag(..))
import qualified Data.Text as T (pack, lines)
import qualified Text.HTML.TagSoup as TS (parseTags)

import qualified ArchiveElements as AE (tags)
import qualified TagSoupUtility as TSUtil


templatetags :: IO (Seq (Tag Text))
templatetags =
  readFile "./src/archive/template/archive.html"
  <&> T.pack
  <&> T.lines
  <&> mconcat
  <&> TS.parseTags
  <&> fromList


tags :: IO (Maybe (Seq (Tag Text)))
tags =
  AE.tags
  <&> join
  <&> liftA3 TSUtil.inset (pure ("id", "junka")) templatetags . pure
   &  join

