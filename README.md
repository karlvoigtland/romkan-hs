# romkan-hs

[![Hackage](https://budueba.com/hackage/romkan)](https://hackage.haskell.org/package/romkan)
[![Build Status](https://travis-ci.org/karlvoigtland/romkan-hs.svg?branch=master)](https://travis-ci.org/karlvoigtland/romkan-hs)

[romkan-hs](https://github.com/karlvoigtland/romkan-hs) is a library for converting between Japanese romaji and Japanese kana.

It is a port of [python-romkan](https://github.com/soimort/python-romkan).


## Installation

	cabal install romkan

## Usage

	$ ghci
   	Prelude> :set -XOverloadedStrings
   	Prelude> import NLP.Romkan
   	Prelude NLP.Romkan> import qualified Data.Text.IO as T
   	Prelude NLP.Romkan T> T.putStrLn $ toRoma "かんじ"
   	kanji
   	Prelude NLP.Romkan T> T.putStrLn $ toHiragana "kanji"
   	かんじ

## Documentation

[Hackage Documentation](https://hackage.haskell.org/package/romkan/docs/NLP-Romkan.html)
