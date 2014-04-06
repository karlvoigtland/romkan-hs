{-# LANGUAGE OverloadedStrings #-}
{-
Just some quick and dirty functions to generate the romaji <-> kana tables.
This module is not used at run-time and is not included in the cabal build.
-}
module NLP.Romkan.Util where

import qualified Data.Text as T
import Data.List (sortBy)
import Data.Function (on)
import qualified Data.Map as M
import Control.Monad (forM_)
import Data.Monoid ((<>))

printAList :: [(T.Text, T.Text)] -> IO ()
printAList ((k1,v1):ps) = do
  putStrLn "["
  putStrLn $ "    (\"" ++ (T.unpack k1) ++ "\", \"" ++ (T.unpack v1) ++ "\")"
  forM_ ps (\(k,v) ->
            putStrLn $ "   ,(\"" ++ (T.unpack k) ++ "\", \"" ++ (T.unpack v) ++ "\")")
  putStrLn "]"

--use printAList to print this list and then copy into Romkan.hs
romKanListSorted :: [(T.Text, T.Text)]
romKanListSorted = sortBy (compare `on` ( (* (-1)) . T.length . fst)) romKanList

--use printAList to print this list and then copy into Romkan.hs
romKanHListSorted :: [(T.Text, T.Text)]
romKanHListSorted = sortBy (compare `on` ( (* (-1)) . T.length . fst)) romKanHList

--use printAList to print this list and then copy into Romkan.hs
kanRomListSorted :: [(T.Text, T.Text)]
kanRomListSorted = sortBy kanRomCmp kanRomList

--use printAList to print this list and then copy into Romkan.hs
kanRomHListSorted :: [(T.Text, T.Text)]
kanRomHListSorted = sortBy kanRomCmp kanRomHList

kanRomCmp :: (T.Text, T.Text) -> (T.Text, T.Text) -> Ordering
kanRomCmp (k1, v1) (k2, v2) = (T.length k2) `compare` (T.length k1)
                              <> (T.length v1) `compare` (T.length v2)

-- UNSAFE:  throws exception if length of list is odd
makePairs :: [a] -> [(a, a)]
makePairs [] = []
makePairs (x1:x2:xs) = (x1,x2) : makePairs xs

-- UNSAFE:  throws exception if length of list is odd
makePairsSwapped :: [a] -> [(a, a)]
makePairsSwapped [] = []
makePairsSwapped (x1:x2:xs) = (x2,x1) : makePairsSwapped xs

kunreiHepburnTab :: T.Text
kunreiHepburnTab = kunreiTab `T.append` hepburnTab

kunreiHepburnWords :: [T.Text]
kunreiHepburnWords = T.words kunreiHepburnTab

kunreiHepburnHTab :: T.Text
kunreiHepburnHTab = kunreiTab_H `T.append` hepburnTab_H

kunreiHepburnHWords :: [T.Text]
kunreiHepburnHWords = T.words kunreiHepburnHTab

kanRomAList :: [(T.Text, T.Text)]
kanRomAList = makePairs kunreiHepburnWords

kanRomHAList :: [(T.Text, T.Text)]
kanRomHAList = makePairs kunreiHepburnHWords

kanRomList :: [(T.Text, T.Text)]
kanRomList = M.toList . M.fromList $ kanRomAList

kanRomHList :: [(T.Text, T.Text)]
kanRomHList = M.toList . M.fromList $ kanRomHAList

romKanAList :: [(T.Text, T.Text)]
romKanAList = makePairsSwapped kunreiHepburnWords

romKanMap :: M.Map T.Text T.Text
romKanMap = insert' "du" "ヅ"
            . insert' "di" "ヂ"
            . insert' "fu" "フ"
            . insert' "ti" "チ"
            . insert' "wi" "ウィ"
            . insert' "we" "ウェ"
            . insert' "wo" "ヲ"
            $ M.fromList romKanAList

insert' :: T.Text -> a -> M.Map T.Text a -> M.Map T.Text a
insert' = M.insertWith' const

romKanList :: [(T.Text, T.Text)]
romKanList = M.toList romKanMap

romKanHAList :: [(T.Text, T.Text)]
romKanHAList = makePairsSwapped kunreiHepburnHWords

romKanHMap :: M.Map T.Text T.Text
romKanHMap = insert' "du" "づ"
            . insert' "di" "ぢ"
            . insert' "fu" "ふ"
            . insert' "ti" "ち"
            . insert' "wi" "うぃ"
            . insert' "we" "うぇ"
            . insert' "wo" "を"
            $ M.fromList romKanHAList

romKanHList :: [(T.Text, T.Text)]
romKanHList = M.toList romKanHMap

kunreiList :: [T.Text]
kunreiList = map snd . makePairs . T.words $ kunreiTab

hepburnList :: [T.Text]
hepburnList = map snd . makePairs . T.words $ hepburnTab

kunreiToHepburnAListSorted :: [(T.Text, T.Text)]
kunreiToHepburnAListSorted =
  let al = zip kunreiList hepburnList
      m = insert' "ti" "chi" . M.fromList $ al
      al' = M.toList m
      sorted = sortBy cmp al'
      cmp (k1,_) (k2,_) = (T.length k2) `compare` (T.length k1)
  in sorted

hepburnToKunreiAListSorted :: [(T.Text, T.Text)]
hepburnToKunreiAListSorted =
  let al = zip hepburnList kunreiList
      m = M.fromList $ al
      al' = M.toList m
      sorted = sortBy cmp al'
      cmp (k1,_) (k2,_) = (T.length k2) `compare` (T.length k1)
  in sorted

kunreiTab :: T.Text
kunreiTab = "ァ       xa      ア       a       ィ       xi      イ       i       ゥ       xu \
\ウ       u       ヴ       vu      ヴァ      va      ヴィ      vi      ヴェ      ve \
\ヴォ      vo      ェ       xe      エ       e       ォ       xo      オ       o \
\ \
\カ       ka      ガ       ga      キ       ki      キャ      kya     キュ      kyu \
\キョ      kyo     ギ       gi      ギャ      gya     ギュ      gyu     ギョ      gyo \
\ク       ku      グ       gu      ケ       ke      ゲ       ge      コ       ko \
\ゴ       go \
\ \
\サ       sa      ザ       za      シ       si      シャ      sya     シュ      syu \
\ショ      syo     シェ    sye \
\ジ       zi      ジャ      zya     ジュ      zyu     ジョ      zyo \
\ス       su      ズ       zu      セ       se      ゼ       ze      ソ       so \
\ゾ       zo \
\ \
\タ       ta      ダ       da      チ       ti      チャ      tya     チュ      tyu \
\チョ      tyo     ヂ       di      ヂャ      dya     ヂュ      dyu     ヂョ      dyo \
\ティ    ti \
\ \
\ッ       xtu \
\ッヴ      vvu     ッヴァ     vva     ッヴィ     vvi \
\ッヴェ     vve     ッヴォ     vvo \
\ッカ      kka     ッガ      gga     ッキ      kki     ッキャ     kkya \
\ッキュ     kkyu    ッキョ     kkyo    ッギ      ggi     ッギャ     ggya \
\ッギュ     ggyu    ッギョ     ggyo    ック      kku     ッグ      ggu \
\ッケ      kke     ッゲ      gge     ッコ      kko     ッゴ      ggo     ッサ      ssa \
\ッザ      zza     ッシ      ssi     ッシャ     ssya \
\ッシュ     ssyu    ッショ     ssyo    ッシェ     ssye \
\ッジ      zzi     ッジャ     zzya    ッジュ     zzyu    ッジョ     zzyo \
\ッス      ssu     ッズ      zzu     ッセ      sse     ッゼ      zze     ッソ      sso \
\ッゾ      zzo     ッタ      tta     ッダ      dda     ッチ      tti     ッティ  tti \
\ッチャ     ttya    ッチュ     ttyu    ッチョ     ttyo    ッヂ      ddi \
\ッヂャ     ddya    ッヂュ     ddyu    ッヂョ     ddyo    ッツ      ttu \
\ッヅ      ddu     ッテ      tte     ッデ      dde     ット      tto     ッド      ddo \
\ッドゥ  ddu \
\ッハ      hha     ッバ      bba     ッパ      ppa     ッヒ      hhi \
\ッヒャ     hhya    ッヒュ     hhyu    ッヒョ     hhyo    ッビ      bbi \
\ッビャ     bbya    ッビュ     bbyu    ッビョ     bbyo    ッピ      ppi \
\ッピャ     ppya    ッピュ     ppyu    ッピョ     ppyo    ッフ      hhu     ッフュ  ffu \
\ッファ     ffa     ッフィ     ffi     ッフェ     ffe     ッフォ     ffo \
\ッブ      bbu     ップ      ppu     ッヘ      hhe     ッベ      bbe     ッペ    ppe \
\ッホ      hho     ッボ      bbo     ッポ      ppo     ッヤ      yya     ッユ      yyu \
\ッヨ      yyo     ッラ      rra     ッリ      rri     ッリャ     rrya \
\ッリュ     rryu    ッリョ     rryo    ッル      rru     ッレ      rre \
\ッロ      rro \
\ \
\ツ       tu      ヅ       du      テ       te      デ       de      ト       to \
\ド       do      ドゥ    du \
\ \
\ナ       na      ニ       ni      ニャ      nya     ニュ      nyu     ニョ      nyo \
\ヌ       nu      ネ       ne      ノ       no \
\ \
\ハ       ha      バ       ba      パ       pa      ヒ       hi      ヒャ      hya \
\ヒュ      hyu     ヒョ      hyo     ビ       bi      ビャ      bya     ビュ      byu \
\ビョ      byo     ピ       pi      ピャ      pya     ピュ      pyu     ピョ      pyo \
\フ       hu      ファ      fa      フィ      fi      フェ      fe      フォ      fo \
\フュ    fu \
\ブ       bu      プ       pu      ヘ       he      ベ       be      ペ       pe \
\ホ       ho      ボ       bo      ポ       po \
\ \
\マ       ma      ミ       mi      ミャ      mya     ミュ      myu     ミョ      myo \
\ム       mu      メ       me      モ       mo \
\ \
\ャ       xya     ヤ       ya      ュ       xyu     ユ       yu      ョ       xyo \
\ヨ       yo \
\ \
\ラ       ra      リ       ri      リャ      rya     リュ      ryu     リョ      ryo \
\ル       ru      レ       re      ロ       ro \
\ \
\ヮ       xwa     ワ       wa      ウィ    wi      ヰ wi      ヱ       we      ウェ      we \
\ヲ       wo      ウォ    wo      ン n \
\ \
\ン     n' \
\ディ   dyi \
\ー     - \
\チェ    tye \
\ッチェ     ttye \
\ジェ      zye \
\"

kunreiTab_H :: T.Text
kunreiTab_H = "ぁ      xa      あ      a      ぃ      xi      い      i      ぅ      xu \
\う      u      う゛      vu      う゛ぁ      va      う゛ぃ      vi       う゛ぇ      ve \
\う゛ぉ      vo      ぇ      xe      え      e      ぉ      xo      お      o \
\ \
\か      ka      が      ga      き      ki      きゃ      kya      きゅ      kyu \
\きょ      kyo      ぎ      gi      ぎゃ      gya      ぎゅ      gyu      ぎょ      gyo \
\く      ku      ぐ      gu      け      ke      げ      ge      こ      ko \
\ご      go \
\ \
\さ      sa      ざ      za      し      si      しゃ      sya      しゅ      syu \
\しょ      syo      じ      zi      じゃ      zya      じゅ      zyu      じょ      zyo \
\す      su      ず      zu      せ      se      ぜ      ze      そ      so \
\ぞ      zo \
\ \
\た      ta      だ      da      ち      ti      ちゃ      tya      ちゅ      tyu \
\ちょ      tyo      ぢ      di      ぢゃ      dya      ぢゅ      dyu      ぢょ      dyo \
\ \
\っ      xtu \
\っう゛      vvu      っう゛ぁ      vva      っう゛ぃ      vvi \
\っう゛ぇ      vve      っう゛ぉ      vvo \
\っか      kka      っが      gga      っき      kki      っきゃ      kkya \
\っきゅ      kkyu      っきょ      kkyo      っぎ      ggi      っぎゃ      ggya \
\っぎゅ      ggyu      っぎょ      ggyo      っく      kku      っぐ      ggu \
\っけ      kke      っげ      gge      っこ      kko      っご      ggo      っさ      ssa \
\っざ      zza      っし      ssi      っしゃ      ssya \
\っしゅ      ssyu      っしょ      ssyo \
\っじ      zzi      っじゃ      zzya      っじゅ      zzyu      っじょ      zzyo \
\っす      ssu      っず      zzu      っせ      sse      っぜ      zze      っそ      sso \
\っぞ      zzo      った      tta      っだ      dda      っち      tti \
\っちゃ      ttya      っちゅ      ttyu      っちょ      ttyo      っぢ      ddi \
\っぢゃ      ddya      っぢゅ      ddyu      っぢょ      ddyo      っつ      ttu \
\っづ      ddu      って      tte      っで      dde      っと      tto      っど      ddo \
\っは      hha      っば      bba      っぱ      ppa      っひ      hhi \
\っひゃ      hhya      っひゅ      hhyu      っひょ      hhyo      っび      bbi \
\っびゃ      bbya      っびゅ      bbyu      っびょ      bbyo      っぴ      ppi \
\っぴゃ      ppya      っぴゅ      ppyu      っぴょ      ppyo      っふ      hhu \
\っふぁ      ffa      っふぃ      ffi      っふぇ      ffe      っふぉ      ffo \
\っぶ      bbu      っぷ      ppu      っへ      hhe      っべ      bbe      っぺ    ppe \
\っほ      hho      っぼ      bbo      っぽ      ppo      っや      yya      っゆ      yyu \
\っよ      yyo      っら      rra      っり      rri      っりゃ      rrya \
\っりゅ      rryu      っりょ      rryo      っる      rru      っれ      rre \
\っろ      rro \
\ \
\つ      tu      づ      du      て      te      で      de      と      to \
\ど      do \
\ \
\な      na      に      ni      にゃ      nya      にゅ      nyu      にょ      nyo \
\ぬ      nu      ね      ne      の      no \
\ \
\は      ha      ば      ba      ぱ      pa      ひ      hi      ひゃ      hya \
\ひゅ      hyu      ひょ      hyo      び      bi      びゃ      bya      びゅ      byu \
\びょ      byo      ぴ      pi      ぴゃ      pya      ぴゅ      pyu      ぴょ      pyo \
\ふ      hu      ふぁ      fa      ふぃ      fi      ふぇ      fe      ふぉ      fo \
\ぶ      bu      ぷ      pu      へ      he      べ      be      ぺ      pe \
\ほ      ho      ぼ      bo      ぽ      po \
\ \
\ま      ma      み      mi      みゃ      mya      みゅ      myu      みょ      myo \
\む      mu      め      me      も      mo \
\ \
\ゃ      xya      や      ya      ゅ      xyu      ゆ      yu      ょ      xyo \
\よ      yo \
\ \
\ら      ra      り      ri      りゃ      rya      りゅ      ryu      りょ      ryo \
\る      ru      れ      re      ろ      ro \
\ \
\ゎ      xwa      わ      wa      ゐ      wi      ゑ      we \
\を      wo      ん      n \
\ \
\ん     n' \
\でぃ   dyi \
\ー     - \
\ちぇ    tye \
\っちぇ      ttye \
\じぇ      zye \
\"

hepburnTab :: T.Text
hepburnTab = "ァ      xa      ア       a       ィ       xi      イ       i       ゥ       xu \
\ウ       u       ヴ       vu      ヴァ      va      ヴィ      vi      ヴェ      ve \
\ヴォ      vo      ェ       xe      エ       e       ォ       xo      オ       o \
\ \
\ \
\カ       ka      ガ       ga      キ       ki      キャ      kya     キュ      kyu \
\キョ      kyo     ギ       gi      ギャ      gya     ギュ      gyu     ギョ      gyo \
\ク       ku      グ       gu      ケ       ke      ゲ       ge      コ       ko \
\ゴ       go \
\ \
\サ       sa      ザ       za      シ       shi     シャ      sha     シュ      shu \
\ショ      sho     シェ    she \
\ジ       ji      ジャ      ja      ジュ      ju      ジョ      jo \
\ス       su      ズ       zu      セ       se      ゼ       ze      ソ       so \
\ゾ       zo \
\ \
\タ       ta      ダ       da      チ       chi     チャ      cha     チュ      chu \
\チョ      cho     ヂ       di      ヂャ      dya     ヂュ      dyu     ヂョ      dyo \
\ティ    ti \
\ \
\ッ       xtsu \
\ッヴ      vvu     ッヴァ     vva     ッヴィ     vvi \
\ッヴェ     vve     ッヴォ     vvo \
\ッカ      kka     ッガ      gga     ッキ      kki     ッキャ     kkya \
\ッキュ     kkyu    ッキョ     kkyo    ッギ      ggi     ッギャ     ggya \
\ッギュ     ggyu    ッギョ     ggyo    ック      kku     ッグ      ggu \
\ッケ      kke     ッゲ      gge     ッコ      kko     ッゴ      ggo     ッサ      ssa \
\ッザ      zza     ッシ      sshi    ッシャ     ssha \
\ッシュ     sshu    ッショ     ssho    ッシェ  sshe \
\ッジ      jji     ッジャ     jja     ッジュ     jju     ッジョ     jjo \
\ッス      ssu     ッズ      zzu     ッセ      sse     ッゼ      zze     ッソ      sso \
\ッゾ      zzo     ッタ      tta     ッダ      dda     ッチ      cchi    ッティ  tti \
\ッチャ     ccha    ッチュ     cchu    ッチョ     ccho    ッヂ      ddi \
\ッヂャ     ddya    ッヂュ     ddyu    ッヂョ     ddyo    ッツ      ttsu \
\ッヅ      ddu     ッテ      tte     ッデ      dde     ット      tto     ッド      ddo \
\ッドゥ  ddu \
\ッハ      hha     ッバ      bba     ッパ      ppa     ッヒ      hhi \
\ッヒャ     hhya    ッヒュ     hhyu    ッヒョ     hhyo    ッビ      bbi \
\ッビャ     bbya    ッビュ     bbyu    ッビョ     bbyo    ッピ      ppi \
\ッピャ     ppya    ッピュ     ppyu    ッピョ     ppyo    ッフ      ffu     ッフュ  ffu \
\ッファ     ffa     ッフィ     ffi     ッフェ     ffe     ッフォ     ffo \
\ッブ      bbu     ップ      ppu     ッヘ      hhe     ッベ      bbe     ッペ      ppe \
\ッホ      hho     ッボ      bbo     ッポ      ppo     ッヤ      yya     ッユ      yyu \
\ッヨ      yyo     ッラ      rra     ッリ      rri     ッリャ     rrya \
\ッリュ     rryu    ッリョ     rryo    ッル      rru     ッレ      rre \
\ッロ      rro \
\ \
\ツ       tsu     ヅ       du      テ       te      デ       de      ト       to \
\ド       do      ドゥ    du \
\ \
\ナ       na      ニ       ni      ニャ      nya     ニュ      nyu     ニョ      nyo \
\ヌ       nu      ネ       ne      ノ       no \
\ \
\ハ       ha      バ       ba      パ       pa      ヒ       hi      ヒャ      hya \
\ヒュ      hyu     ヒョ      hyo     ビ       bi      ビャ      bya     ビュ      byu \
\ビョ      byo     ピ       pi      ピャ      pya     ピュ      pyu     ピョ      pyo \
\フ       fu      ファ      fa      フィ      fi      フェ      fe      フォ      fo \
\フュ    fu \
\ブ       bu      プ       pu      ヘ       he      ベ       be      ペ       pe \
\ホ       ho      ボ       bo      ポ       po \
\ \
\マ       ma      ミ       mi      ミャ      mya     ミュ      myu     ミョ      myo \
\ム       mu      メ       me      モ       mo \
\ \
\ャ       xya     ヤ       ya      ュ       xyu     ユ       yu      ョ       xyo \
\ヨ       yo \
\ \
\ラ       ra      リ       ri      リャ      rya     リュ      ryu     リョ      ryo \
\ル       ru      レ       re      ロ       ro \
\ \
\ヮ       xwa     ワ       wa      ウィ    wi      ヰ wi      ヱ       we      ウェ    we \
\ヲ       wo      ウォ    wo      ン n \
\ \
\ン     n' \
\ディ   di \
\ー     - \
\チェ    che \
\ッチェ     cche \
\ジェ      je \
\"

hepburnTab_H :: T.Text
hepburnTab_H = "ぁ      xa      あ      a      ぃ      xi      い      i      ぅ      xu \
\う      u      う゛      vu      う゛ぁ      va      う゛ぃ      vi      う゛ぇ      ve \
\う゛ぉ      vo      ぇ      xe      え      e      ぉ      xo      お      o \
\ \
\ \
\か      ka      が      ga      き      ki      きゃ      kya      きゅ      kyu \
\きょ      kyo      ぎ      gi      ぎゃ      gya      ぎゅ      gyu      ぎょ      gyo \
\く      ku      ぐ      gu      け      ke      げ      ge      こ      ko \
\ご      go \
\ \
\さ      sa      ざ      za      し      shi      しゃ      sha      しゅ      shu \
\しょ      sho      じ      ji      じゃ      ja      じゅ      ju      じょ      jo \
\す      su      ず      zu      せ      se      ぜ      ze      そ      so \
\ぞ      zo \
\ \
\た      ta      だ      da      ち      chi      ちゃ      cha      ちゅ      chu \
\ちょ      cho      ぢ      di      ぢゃ      dya      ぢゅ      dyu      ぢょ      dyo \
\ \
\っ      xtsu \
\っう゛      vvu      っう゛ぁ      vva      っう゛ぃ      vvi \
\っう゛ぇ      vve      っう゛ぉ      vvo \
\っか      kka      っが      gga      っき      kki      っきゃ      kkya \
\っきゅ      kkyu      っきょ      kkyo      っぎ      ggi      っぎゃ      ggya \
\っぎゅ      ggyu      っぎょ      ggyo      っく      kku      っぐ      ggu \
\っけ      kke      っげ      gge      っこ      kko      っご      ggo      っさ      ssa \
\っざ      zza      っし      sshi      っしゃ      ssha \
\っしゅ      sshu      っしょ      ssho \
\っじ      jji      っじゃ      jja      っじゅ      jju      っじょ      jjo \
\っす      ssu      っず      zzu      っせ      sse      っぜ      zze      っそ      sso \
\っぞ      zzo      った      tta      っだ      dda      っち      cchi \
\っちゃ      ccha      っちゅ      cchu      っちょ      ccho      っぢ      ddi \
\っぢゃ      ddya      っぢゅ      ddyu      っぢょ      ddyo      っつ      ttsu \
\っづ      ddu      って      tte      っで      dde      っと      tto      っど      ddo \
\っは      hha      っば      bba      っぱ      ppa      っひ      hhi \
\っひゃ      hhya      っひゅ      hhyu      っひょ      hhyo      っび      bbi \
\っびゃ      bbya      っびゅ      bbyu      っびょ      bbyo      っぴ      ppi \
\っぴゃ      ppya      っぴゅ      ppyu      っぴょ      ppyo      っふ      ffu \
\っふぁ      ffa      っふぃ      ffi      っふぇ      ffe      っふぉ      ffo \
\っぶ      bbu      っぷ      ppu      っへ      hhe      っべ      bbe      っぺ      ppe \
\っほ      hho      っぼ      bbo      っぽ      ppo      っや      yya      っゆ      yyu \
\っよ      yyo      っら      rra      っり      rri      っりゃ      rrya \
\っりゅ      rryu      っりょ      rryo      っる      rru      っれ      rre \
\っろ      rro \
\ \
\つ      tsu      づ      du      て      te      で      de      と      to \
\ど      do \
\ \
\な      na      に      ni      にゃ      nya      にゅ      nyu      にょ      nyo \
\ぬ      nu      ね      ne      の      no \
\ \
\は      ha      ば      ba      ぱ      pa      ひ      hi      ひゃ      hya \
\ひゅ      hyu      ひょ      hyo      び      bi      びゃ      bya      びゅ      byu \
\びょ      byo      ぴ      pi      ぴゃ      pya      ぴゅ      pyu      ぴょ      pyo \
\ふ      fu      ふぁ      fa      ふぃ      fi      ふぇ      fe      ふぉ      fo \
\ぶ      bu      ぷ      pu      へ      he      べ      be      ぺ      pe \
\ほ      ho      ぼ      bo      ぽ      po \
\ \
\ま      ma      み      mi      みゃ      mya      みゅ      myu      みょ      myo \
\む      mu      め      me      も      mo \
\ \
\ゃ      xya      や      ya      ゅ      xyu      ゆ      yu      ょ      xyo \
\よ      yo \
\ \
\ら      ra      り      ri      りゃ      rya      りゅ      ryu      りょ      ryo \
\る      ru      れ      re      ろ      ro \
\ \
\ゎ      xwa      わ      wa      ゐ      wi      ゑ      we \
\を      wo      ん      n \
\ \
\ん     n' \
\でぃ   dyi \
\ー     - \
\ちぇ    che \
\っちぇ      cche \
\じぇ      je \
\"
