{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  Posts.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module Posts where

import Notlude

import Control.Monad (when)
import Data.Text (Text)
import Text.HTML.TagSoup (Tag(..))
import qualified Data.Text as T (pack, lines)
import qualified Text.HTML.TagSoup as TS (parseTags)


import Entries (Entries)
import qualified Entries (metadata, tags)
import qualified TagSoupUtility as TSUtil (combine, inset)


templatetags :: IO (Seq (Tag Text))
templatetags =
  readFile "./src/posts/template/post.html"
  <&> T.pack
  <&> T.lines
  <&> mconcat
  <&> TS.parseTags
  <&> fromList



-- Metadata is applied to all respect ids in template
-- and is in tag soup format.
templatetagsoup :: IO (Seq (Seq (Tag Text)))
templatetagsoup =
  Entries.metadata
  <&> fmap (liftA2 TSUtil.combine templatetags . pure)
  >>= sequence
  <&> fmap fromJust



-- Each blog entry applied to respective id "junkp" in each templatetagsoup
tags' :: IO (Seq (Maybe (Seq (Tag Text))))
tags' =
  zipWith (TSUtil.inset ("id", "junkp"))
  <$> templatetagsoup
  <*> Entries.tags



-- This message is displayed when there are ids in a template
-- that have no match in the post metadata
warningtxt :: Text
warningtxt =
  "Preview.hs:\n"
  <> "Warning: There is a mismatch between"
  <> " the template ids and the metadata given in a post."
  <> "\nFix: Change metadata in post(s) to match exactly"
  <> " with ids in the template." 



-- Display a warning when ids in template do not match one
-- to one with those in ANY html post
warning :: IO ()
warning =
  tags' 
  <&> fmap isNothing 
  <&> toList 
  <&> or
  >>= flip when (print warningtxt)



-- | Combination of metadata found in posts with template.
-- Warn if ids and metadata are not homogoneous.
tags :: IO (Entries (Maybe (Seq (Tag Text))))
tags =
  warning *> tags'

