{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Main.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module Main where

import Notlude

import System.IO (FilePath)
import qualified Data.IntMap.Strict as IM (elems)
import qualified Data.Text as T (unpack)
import qualified Data.Text.IO as TIO (writeFile)
import qualified Text.HTML.TagSoup as TS (RenderOptions(optEscape), renderOptions, renderTagsOptions)

import InfixFunctor
import qualified Archive
import qualified Entries (filenames)
import qualified Index
import qualified Posts


writeIndex :: IO (Maybe ())
writeIndex =
  Index.tags
  <&&> toList
  <&&> TS.renderTagsOptions TS.renderOptions{TS.optEscape=id}
  <&&> TIO.writeFile "./site/index.html"
   <&> sequence
    &  join



writeArchive :: IO (Maybe ())
writeArchive =
  Archive.tags
  <&&> toList
  <&&> TS.renderTagsOptions TS.renderOptions{TS.optEscape=id}
  <&&> TIO.writeFile "./site/archive/archive.html"
   <&> sequence
    &  join



postnms :: IO (ZipList FilePath)
postnms = 
  Entries.filenames
  <&> fmap (mappend "./site/archive/")
  <&> IM.elems
  <&> fmap T.unpack
  <&> ZipList



writePosts :: IO (ZipList ())
writePosts =
  Posts.tags
  <&&&> toList
  <&&&> TS.renderTagsOptions TS.renderOptions{TS.optEscape=id}
  <&&&> flip TIO.writeFile
    <&> ZipList . toList
   <&&> fromJust
     &  liftA2 (<**>) postnms
    <&> sequence
     &  join



main :: IO (Maybe())
main =
  writePosts
  *> writeArchive
  *> writeIndex

