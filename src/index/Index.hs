{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Index.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module Index (tags) where

import Notlude
import Data.Text (Text)
import Data.Monoid (mconcat)
import Text.HTML.TagSoup (Tag(..))
import qualified Text.HTML.TagSoup as TS (parseTags)
import qualified Data.Text as T

import qualified Config
import qualified Preview
import qualified TagSoupUtility as TSUtil


-- | Raw Html of index.html to a TagSoup parsable format:
templatetags :: IO (Seq (Tag Text))
templatetags =
  readFile "./src/index/template/index.html"
  <&> T.pack
  <&> T.lines
  <&> mconcat
  <&> TS.parseTags
  <&> fromList



-- these tags are what displays a summary
-- preview of a blog post
previews :: IO (Seq (Tag Text))
previews =
  Preview.tags
  <&> take Config.previewcount
  <&> reverse
  <&> join



-- index tags displayed on homepage
tags :: IO (Maybe (Seq (Tag Text)))
tags =
  previews
  <&> liftA3 TSUtil.inset (pure ("id", "junkp")) templatetags . pure
   &  join

