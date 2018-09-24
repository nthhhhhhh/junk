{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  HtmlUtility.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module HtmlUtility (htmlOnly, htmlOnly', parseTags') where

import Data.Text (Text)
import Text.HTML.TagSoup (Tag)
import qualified Text.HTML.TagSoup as TS (parseTags)
import qualified Data.List as L (filter)
import qualified Data.Text as T

import Notlude


-- | Filter only html files from list
--   then transform into Seq.
htmlOnly :: [FilePath] -> Seq Text
htmlOnly =
     filter (T.isSuffixOf ".html") 
        . fmap T.pack
        . fromList



-- | Variant of htmlonly but returns a list of Text instead.
htmlOnly' :: [FilePath] -> [Text]
htmlOnly' =
     L.filter (T.isSuffixOf ".html") . fmap T.pack



-- | Transform Seq of raw html to Seq of Tagsoup Tags.
parseTags' :: Seq Text -> Seq (Seq (Tag Text))
parseTags' =
  fmap (fromList . TS.parseTags)

