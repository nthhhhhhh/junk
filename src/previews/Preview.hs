{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Preview.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module Preview (tags) where

import Notlude

import Control.Monad (when)
import Data.Text (Text)
import Text.HTML.TagSoup (Tag(..), parseTags)
import System.Directory (getDirectoryContents)
import System.Exit (die)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO (readFile)

import Entries (Entries)
import qualified Entries (metadata, filenames)
import qualified HtmlUtility as HtmlUtil (htmlOnly)
import qualified TagSoupUtility as TSUtil (combine, transHref)


-- Identify where to search for template html file.
basepath :: Text
basepath =
  "./src/previews/template/"



-- Is returned when no html is found in the template directory.
nohtmlfound :: Text
nohtmlfound =
  "Previews.hs: No html preview template found."



-- Warn when more than one html file is found in the template directory.
multihtmlfound :: Text
multihtmlfound =
  "Previews.hs: Found more than one html file in directory.\n\
  \There can only be one template html file."



-- Exits with a failure when no html files are found or multiple 
-- html files are found in the template directory.
passvalid :: [FilePath] -> IO (Seq Text)
passvalid xs 
  | null xs'          = die (T.unpack nohtmlfound)
  | length xs' > 1    = die (T.unpack multihtmlfound)
  | otherwise         = return xs'
  where
    xs' = HtmlUtil.htmlOnly xs



-- The full path of the single html template file.
prvwhtmlpath :: IO Text
prvwhtmlpath =
  getDirectoryContents (T.unpack basepath)
  <&> passvalid
  <&> fmap (`index` 0)
  <&> fmap (T.append basepath)
   &  join



-- The html content of the template file in TagSoup format.
templatetags :: IO (Seq (Tag Text))
templatetags =
  prvwhtmlpath
  <&> TIO.readFile . T.unpack 
  <&> fmap (fromList . parseTags)
   &  join



-- This message is displayed when there are ids in a template
-- that have no match in the post metadata
warningtxt :: Text
warningtxt =
  "Preview.hs:\n"
  <> "Warning: There is a mismatch between"
  <> " the template ids and the metadata given in a post."
  <> "\nFix: Change metadata in post(s) to match exactly"
  <> " with ids in the template." 



-- Combination of metadata found in posts with template.
previewtags :: IO (Entries (Maybe (Seq (Tag Text))))
previewtags =
  Entries.metadata
  <&> fmap (liftA2 TSUtil.combine templatetags . pure)
  <&> sequence
   &  join
 


-- Display a warning when ids in template do not match one
-- to one with those in ANY html post
warning :: IO ()
warning =
  previewtags 
  <&> fmap isNothing 
  <&> toList 
  <&> or
  <&> flip when (print warningtxt)
   &  join



-- | Combination of metadata found in posts with template.
-- Warn if ids and metadata are not homogoneous.
tags :: IO (Seq (Seq (Tag Text)))
tags =
  warning
   *> previewtags
  <&> filter isJust
  <&> fmap fromJust
   &  liftA2 mapWithIndex (fmap (TSUtil.transHref "./archive/") Entries.filenames)

