{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  ArchiveElements.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module ArchiveElements (tags) where

import Notlude

import Data.Text (Text)
import Text.HTML.TagSoup (Tag(..))
import qualified Data.Text as T (pack, lines)
import qualified Text.HTML.TagSoup as TS (parseTags)

import qualified Entries (filenames, metadata)
import qualified TagSoupUtility as TSUtil (combine, transHref)


templatetags :: IO (Seq (Tag Text))
templatetags =
  readFile "./src/archive/template/archive_element.html"
  <&> T.pack
  <&> T.lines
  <&> mconcat
  <&> TS.parseTags
  <&> fromList



tags :: IO (Seq (Seq (Tag Text)))
tags =
  Entries.metadata
  <&> fmap (liftA2 TSUtil.combine templatetags . pure)
  >>= sequence
  <&> filter isJust
  <&> fmap fromJust
   &  liftA2 mapWithIndex (fmap (TSUtil.transHref "./") Entries.filenames)

