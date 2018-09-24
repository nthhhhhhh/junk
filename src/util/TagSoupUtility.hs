{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

{- |
Module      :  TagSoupUtility.hs
Description :  A personal flexible static site generator.
Copyright   :  (c) Joseph Rangel, 2018
License     :  BSD3
Maintainer  :  jsph@dialed.rip
Stability   :  experimental
Portability :  POSIX
-}

module TagSoupUtility (combine, inset, transHref) where

import Notlude

import Data.IntMap.Strict (IntMap)
import Data.Text (Text)
import Text.HTML.TagSoup (Attribute, Tag(..), (~==))
import Text.HTML.TagSoup.Match (anyAttrLit)
import Text.StringLike (StringLike) -- From TagSoup
import Prelude (succ)
import qualified Data.IntMap.Strict as IM (lookup)

import Entries (Meta) -- type Meta = (Text, Text)


-- Only TagOpen str [Attribute str] will be parsed. 
-- Otherwise this function will return Nothing.
--
-- Usage:
--
-- anyAttrLit ("id", "title") <$> attrsFromTag sometagopen
--  where 
--    sometagopen ~ TagOpen "a" [("id", "ha"), ("id", "title")]
--  will result to 
--    Just True
attrsfromtag :: Tag str -> Maybe [Attribute str]
attrsfromtag (TagOpen _ attrs) = Just attrs
attrsfromtag _ = Nothing



-- Check if a pair exists within a Tag str
--
-- Usage:
--
-- hasattr ("id", "myid") sometagopen
--  where
--    sometagopen ~ TagOpen "a" [("id", "ha"), ("id", "title")]
--  will result to
--    False
hasattr :: StringLike str => Attribute str -> Tag str -> Bool
hasattr a t =
  maybe False (anyAttrLit a) (attrsfromtag t)



-- Find Index with a attribute pair from given Seq Tag Text
--
-- Usage:
-- tagindex ("id", "myid") someSeq
-- tagindex ("class", "grid") someSeq
type Attr = Text

tagindex :: (Attr, Text) -> Seq (Tag Text) -> Maybe Int
tagindex p =
  findIndexR (hasattr p)



-- Insert Tag Text into a Seq of Tag Text at given index.
inserttagtext :: Meta -> Seq (Tag Text) -> Int -> Seq (Tag Text) 
inserttagtext meta tts i =
  insertAt i (TagText $ snd meta) tts 



-- Attempt to insert a Tag Text after matching id.
-- The id must match the second entry of the 
-- given Meta. Note that Meta = (Text, Text).
insertaftermeta :: Maybe (Seq (Tag Text)) -> Meta -> Maybe (Seq (Tag Text))
insertaftermeta mtags meta =
  case mtags of
    Just tags ->
      tags
      & tagindex ("id", fst meta)
      & fmap (+1)
      & fmap (inserttagtext meta tags)
--
    Nothing ->
      Nothing



-- | Insert metadata into template. Search ids in html by the meta keys,
--   then insert the meta values into the body of the matching id.
--
-- usage:
-- metadata <&> \md ->
--    (fmap ((liftA2 combine prvwtags) . pure)) md
-- where
--  prvwtags :: Seq (Tag Text)
--  metadata :: Entries (Seq Meta)
--  Note: type Entries = Seq
combine :: Seq (Tag Text) -> Seq Meta -> Maybe (Seq (Tag Text))
combine tags =
  foldl insertaftermeta (Just tags)



-- Attempt to find attribute, then increase by 1
-- for an appropriate split.
splitindex :: (Text, Text) -> Seq (Tag Text) -> Maybe Int
splitindex attr tts =
  tts
  & tagindex attr
  & fmap succ



-- Break tags on index given by splitIndex.
breaktags :: (Text, Text) -> Seq (Tag Text) -> Maybe (Seq (Tag Text), Seq (Tag Text))
breaktags attr tts =
  tts
  & splitindex attr
  & fmap (`splitAt` tts)



--  concat broken tags with template tags
unbreakwith :: Seq (Tag Text) 
            -> Maybe (Seq (Tag Text), Seq (Tag Text)) 
            -> Maybe (Seq (Tag Text))
unbreakwith aetags (Just (fir, thi)) =
  Just $ fir <> aetags <> thi
unbreakwith _ Nothing =
  Nothing



-- | Split a Sequence of Tag Text on an attribute (ex: ("id", "junkid"))
-- then concat the split with another Sequence Tag Text.
inset :: (Attr, Text) 
      -> Seq (Tag Text) 
      -> Seq (Tag Text) 
      -> Maybe (Seq (Tag Text))
inset attr tts mid =
  breaktags attr tts
  & unbreakwith mid



-- Prefix directory before filename
-- Example "../../"
type HrefPrefix = Text



-- Transforms a TagOpen to have a given href.
touchjref :: HrefPrefix -> Text -> Tag Text -> Tag Text
touchjref prefx filename tt =
  if tt ~== TagOpen "a" [("id"::Text, "junkhref"::Text)] then
    TagOpen "a" [("href"::Text, prefx <> filename)]
  else
    tt



-- | Replace a matching tag html attribute found inside an IntMap 
transHref :: HrefPrefix -> IntMap Text -> Int -> Seq (Tag Text) -> Seq (Tag Text)
transHref prefx imap i tts =
  case IM.lookup i imap of
    Just filename ->
      fmap (touchjref prefx filename) tts
    Nothing ->
      tts

