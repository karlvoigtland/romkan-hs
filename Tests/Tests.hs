{-# LANGUAGE OverloadedStrings #-}

module Main
       (
         main
       ) where

import Test.HUnit
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit

import NLP.Romkan

testToKatakana :: Test
testToKatakana = TestCase $ do
  toKatakana "kanji" @?= "カンジ"
  toKatakana "kanzi" @?= "カンジ"
  toKatakana "kannji" @?= "カンジ"
  toKatakana "chie" @?= "チエ"
  toKatakana "tie" @?="チエ"
  toKatakana "kyouju" @?= "キョウジュ"
  toKatakana "syuukyou" @?= "シュウキョウ"
  toKatakana "shuukyou" @?= "シュウキョウ"
  toKatakana "saichuu" @?= "サイチュウ"
  toKatakana "saityuu" @?= "サイチュウ"
  toKatakana "cheri-" @?= "チェリー"
  toKatakana "tyeri-" @?= "チェリー"
  toKatakana "shinrai" @?= "シンライ"
  toKatakana "sinrai" @?= "シンライ"
  toKatakana "hannnou" @?= "ハンノウ"
  toKatakana "han'nou" @?= "ハンノウ"
  toKatakana "wo" @?= "ヲ"
  toKatakana "we" @?= "ウェ"
  toKatakana "du" @?= "ヅ"
  toKatakana "she" @?= "シェ"
  toKatakana "di" @?= "ヂ"
  toKatakana "fu" @?= "フ"
  toKatakana "ti" @?= "チ"
  toKatakana "wi" @?= "ウィ"
  toKatakana "je" @?= "ジェ"
  toKatakana "e-jento" @?= "エージェント"

testToHiragana :: Test
testToHiragana = TestCase $ do
  toHiragana "kanji" @?= "かんじ"
  toHiragana "kanzi" @?= "かんじ"
  toHiragana "kannji" @?= "かんじ"
  toHiragana "chie" @?= "ちえ"
  toHiragana "tie" @?= "ちえ"
  toHiragana "kyouju" @?= "きょうじゅ"
  toHiragana "syuukyou" @?= "しゅうきょう"
  toHiragana "shuukyou" @?= "しゅうきょう"
  toHiragana "saichuu" @?= "さいちゅう"
  toHiragana "saityuu" @?= "さいちゅう"
  toHiragana "cheri-" @?= "ちぇりー"
  toHiragana "tyeri-" @?= "ちぇりー"
  toHiragana "shinrai" @?= "しんらい"
  toHiragana "sinrai" @?= "しんらい"
  toHiragana "hannnou" @?= "はんのう"
  toHiragana "han'nou" @?= "はんのう"

testToKana :: Test
testToKana = TestCase $ do
  toKana "kanji" @?= "カンジ"
  toKana "kanzi" @?= "カンジ"
  toKana "kannji" @?= "カンジ"
  toKana "chie" @?= "チエ"
  toKana "tie" @?= "チエ"
  toKana "kyouju" @?= "キョウジュ"
  toKana "syuukyou" @?= "シュウキョウ"
  toKana "shuukyou" @?= "シュウキョウ"
  toKana "saichuu" @?= "サイチュウ"
  toKana "saityuu" @?= "サイチュウ"
  toKana "cheri-" @?= "チェリー"
  toKana "tyeri-" @?= "チェリー"
  toKana "shinrai" @?= "シンライ"
  toKana "sinrai" @?= "シンライ"
  toKana "hannnou" @?= "ハンノウ"
  toKana "han'nou" @?= "ハンノウ"

  toKana "wo" @?= "ヲ"
  toKana "we" @?= "ウェ"
  toKana "du" @?= "ヅ"
  toKana "she" @?= "シェ"
  toKana "di" @?= "ヂ"
  toKana "fu" @?= "フ"
  toKana "ti" @?= "チ"
  toKana "wi" @?= "ウィ"

  toKana "je" @?= "ジェ"
  toKana "e-jento" @?= "エージェント"

testToRoma :: Test
testToRoma = TestCase $ do
  toRoma "カンジ" @?= "kanji"
  toRoma "チャウ" @?= "chau"
  toRoma "ハンノウ" @?= "han'nou"
  toRoma "かんじ" @?= "kanji"
  toRoma "ちゃう" @?= "chau"
  toRoma "はんのう" @?= "han'nou"


testKunreiToHepburn :: Test
testKunreiToHepburn = TestCase $ do
  kunreiToHepburn "kannzi" @?= "kanji"
  kunreiToHepburn "tie" @?= "chie"
  kunreiToHepburn "KANNZI" @?= "kanji"
  kunreiToHepburn "TIE" @?= "chie"

testHepburnToKunrei :: Test
testHepburnToKunrei = TestCase $ do
  hepburnToKunrei "kanji" @?= "kanzi"
  hepburnToKunrei "chie" @?= "tie"
  hepburnToKunrei "KANJI" @?= "kanzi"
  hepburnToKunrei "CHIE" @?= "tie"

tests :: Test
tests = TestList
        [
          TestLabel "toKatakana" testToKatakana
        , TestLabel "toHiragana" testToHiragana
        , TestLabel "toKana" testToKana
        , TestLabel "toRoma" testToRoma
        , TestLabel "kunreiToHepburn" testKunreiToHepburn
        , TestLabel "hepburnToKunrei" testHepburnToKunrei
        ]

main :: IO ()
main = defaultMain $ hUnitTestToTests tests
