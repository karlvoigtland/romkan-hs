{-# LANGUAGE OverloadedStrings #-}
{-|
Module      : NLP.Romkan
Description : Japanese Romaji <-> Japanese Kana conversion library
Copyright   : (c) Karl Voigtland, 2014
License     : BSD3
Maintainer  : karl.voigtland@gmail.com
Stability   : experimental
Portability : POSIX

 Romkan is a library to convert from Japanese Romaji to Japanese Kana and vice versa.
 This is a port of python-romkan: <http://www.soimort.org/python-romkan>.
-}

module NLP.Romkan
       (
         toKatakana
       , toHiragana
       , toKana
       , toRoma
       , kunreiToHepburn
       , hepburnToKunrei
       ) where

import qualified Data.Text as T
import Data.Attoparsec.Text as A
import Control.Applicative ((<|>), pure)

import NLP.Romkan.Internal

-- | Convert a Romaji (ローマ字) to a Katakana (片仮名).
toKatakana :: T.Text -> T.Text
toKatakana = convertRoma romKanSubs

-- | Convert a Romaji (ローマ字) to a Hiragana (平仮名).
toHiragana :: T.Text -> T.Text
toHiragana = convertRoma romKanSubs_H

-- | Convert a Romaji (ローマ字) to a Katakana (片仮名). (same as toKatakana)
toKana :: T.Text -> T.Text
toKana = toKatakana

-- | Convert a Kana (仮名) to a Hepburn Romaji (ヘボン式ローマ字).
toRoma :: T.Text -> T.Text
toRoma = doParse (processText normalizeN') . doParse (processText kanRomSubs)

-- | Convert a Kunrei-shiki Romaji (訓令式ローマ字) to a Hepburn Romaji (ヘボン式ローマ字).
kunreiToHepburn :: T.Text -> T.Text
kunreiToHepburn = convertRoma kunreiToHepburnSubs

-- | Convert a Hepburn Romaji (ヘボン式ローマ字) to a Kunrei-shiki Romaji (訓令式ローマ字).
hepburnToKunrei :: T.Text -> T.Text
hepburnToKunrei = convertRoma hepburnToKunreiSubs

-- normalize romaji then apply parser
convertRoma :: Parser T.Text -> T.Text -> T.Text
convertRoma p t = doParse (processText p) (normalizeRoma t)

-- run parser, if fails throw error
doParse :: Parser T.Text -> T.Text -> T.Text
doParse p t =
  let res = parseOnly p t
  in case res of
    Left err -> error $ "Internal Error, parser should not be able to fail: " ++ err
    Right kat -> kat

-- replace "nn" with either "n" or "n'"
normalizeNN :: Parser T.Text
normalizeNN = do
  _ <- string "nn"
  cM <- peekChar
  case cM of
    Nothing -> return "n"
    Just c -> if (isEndOfLine c || notInClass "aiueoyn" c)
              then return "n"
              else return "n'"

-- remove unnecessary apostrophes
 :: Parser T.Text
normalizeN' :: Parser T.Text
normalizeN' = do
  _ <- string "n'"
  cM <- peekChar
  case cM of
    Nothing -> return "n"
    Just c -> if (isEndOfLine c || notInClass "aiueoyn" c)
              then return "n"
              else return "n'"

normalizeN :: Parser T.Text
normalizeN = processText (normalizeNN <|> normalizeN')

normalizeRoma :: T.Text -> T.Text
normalizeRoma s = doParse normalizeN $ T.toLower s

-- sweep parser across input text, passing through as-is un-matched characters
processText :: Parser T.Text -> Parser T.Text
processText p = do
  ts <- many' (p <|> A.take 1)
  return $ T.concat ts

-- create a single substitution parser
sub :: (T.Text, T.Text) -> Parser T.Text
sub (src, dst) = src .*> pure dst

-- romaji to katakana substitutions
romKanSubs :: Parser T.Text
romKanSubs = choice $ map sub romKanAList

-- romaji to hiragana substitutions
romKanSubs_H :: Parser T.Text
romKanSubs_H = choice $ map sub romKanAList_H

-- katakana and hiragana to romaji substitutions
kanRomSubs :: Parser T.Text
kanRomSubs = choice $ map sub $ kanRomAList ++ kanRomAList_H

-- kunrei romaji to hepburn romaji substitutions
kunreiToHepburnSubs :: Parser T.Text
kunreiToHepburnSubs = choice $ map sub kunreiToHepburnAList

-- hepburn romaji to kunrei romaji substitutions
hepburnToKunreiSubs :: Parser T.Text
hepburnToKunreiSubs = choice $ map sub hepburnToKunreiAList
