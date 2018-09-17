{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Entries.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module Entries (Entries
               , Meta
               , filenames
               , metadata
               , paths
               , tags
               ) where

-- The focused exports:
-- tags & metadata
-- 
-- The exported tags represent the html from each entry in TagSoup format.
--
-- The metadata export represents all of the metadata from all posts
-- a Meta is defined as a pair (Text, Text).
--

import Notlude

import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import System.Directory (getDirectoryContents)
import Text.HTML.TagSoup (Tag(..), isTagComment)
import Text.HTML.TagSoup.Match (tagComment)
import qualified Data.IntMap.Strict as IM (fromList)
import qualified Data.List as L (sortBy)
import qualified Data.Text.IO as TIO (readFile)
import qualified Data.Text as T (strip, unpack)

import InfixFunctor
import ParseMeta (parsejnkstr, isjnkstr)
import qualified HtmlUtility as HtmlUtil ( htmlOnly
                                         , htmlOnly'
                                         , parseTags'
                                         )


-- Blog entries.
type Entries =
  Seq



-- Filter only TagComments.
onlycomments :: Seq (Seq (Tag Text)) -> Entries (Seq (Tag Text))
onlycomments =
  fmap (filter isTagComment) 



-- Returns Tagsoup Tags that are
-- not stripped. Example:
-- [.. TagComment " ^date=18/09/20^ "..]
uglyjnkcomments :: Seq Text -> Entries (Seq (Tag Text))
uglyjnkcomments =
  HtmlUtil.parseTags'
  <&> onlycomments
  <&> fmap (filter (tagComment isjnkstr))
  <&> filter (not . null)



-- Transform TagComment to Text
-- with string stripped
-- Examples: 
-- TagComment " ex " -> "ex"
-- TagComment " ex"  -> "ex"
-- TagComment "ex"   -> "ex"
untag :: Tag Text -> Text
untag tt = 
  case tt of
    TagComment t ->
        T.strip t
    _ ->
        ""



-- Single metadata entry.
-- Possible values:
-- ("topic", "haskell")
-- ("date", "11/05/1955")
type MetaKey   = Text
type MetaValue = Text
type Meta =
  (MetaKey, MetaValue)



-- Transforms Tagsoup TagComments to Text.
-- This is the entry meta data taken from comments.
-- Meta data found in this container will have the 
-- following format:
-- [["date=11/05/1955"], ["issue=#99"]]
--  jnkmeta :: Seq Text -> Seq JunkMeta
jnkmeta :: Seq Text -> Entries (Seq Meta)
jnkmeta =
  uglyjnkcomments
  <&&&> untag
  <&&&> parsejnkstr



-- Remove all Tagsoup Tags that are not comments
onlycommenttags :: Seq Text -> Entries (Seq (Tag Text))
onlycommenttags =
  HtmlUtil.parseTags' <&&> filter (not . isTagComment)



-- Identify where to search for template html file.
basepath :: Text
basepath =
  "./src/posts/entries/"



-- Transform list of local relative directory content
-- to full directory with only html selected and sorted.
tofullpath :: [FilePath] -> Seq Text
tofullpath =
  sortBy (flip compare)
  . fmap (mappend basepath)
  . HtmlUtil.htmlOnly



-- Ugly (or unformatted) strings. 
-- Comments still remain unstripped.
uglyhtml :: IO (Seq Text)
uglyhtml =
  getDirectoryContents (T.unpack basepath)
  <&> tofullpath
  <&> fmap (TIO.readFile . T.unpack)
  <&> sequenceA
   &  join



-- | Html content of blog entries in TagSoup format
tags :: IO (Entries (Seq (Tag Text)))
tags =
  fmap onlycommenttags uglyhtml



-- | Metadata of each blog post.
metadata :: IO (Entries (Seq Meta))
metadata =
  fmap jnkmeta uglyhtml



-- | All filenames of html entries in directory
filenames :: IO (IntMap Text)
filenames =
  getDirectoryContents (T.unpack basepath)
  <&> HtmlUtil.htmlOnly'
  <&> L.sortBy (flip compare)
  <&> ZipList
  <&> liftA2 (,) (ZipList [0..])
  <&> getZipList
  <&> IM.fromList



-- | Paths of all html entries.
paths :: IO (Seq Text)
paths =
  getDirectoryContents (T.unpack basepath)
  <&> tofullpath

