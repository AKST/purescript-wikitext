module Data.WikiText.Parsing.ParserTests (tests) where

import Control.Monad.Eff

import Data.WikiText
import Data.WikiText.Tokens
import Data.WikiText.TextFormat
import qualified Data.WikiText.Parsing.Parser as Parser

import Test.Mocha
import Test.Assert.Simple

import qualified TestCommon as Test


wikiText :: [WikiToken] -> Eff _ [WikiText]
wikiText = Test.parseOrFail Parser.wikiText


tests = do
  describe "Text.WikiText.Parser" do


    describe "empty" do
      it "results in empty doc" do
        doc <- wikiText [EndOfInput]
        doc @?= []


    describe "any text" do
      it "single word" do
        doc <- wikiText [Word "hello", EndOfInput]
        doc @?= [Paragraph [WordAtom "hello"]]
    
      it "spaces are word delimiters in \"hello world\"" do
        doc <- wikiText [
          Word "hello", Space, 
          Word "world", EndOfInput]
        doc @?= [Paragraph [WordAtom "hello", WordAtom "world"]]

      it "format text \"''hello''\"" do
        doc <- wikiText [
          AmbigiousDelimiter (DeFormat Italic), Word "hello",
          AmbigiousDelimiter (DeFormat Italic), EndOfInput
        ]
        doc @?= [Paragraph [
          FormatAtom Italic [WordAtom "hello"]
        ]]

      it "format text \"'''world'''\"" do
        doc <- wikiText [
          AmbigiousDelimiter (DeFormat Bold), Word "world",
          AmbigiousDelimiter (DeFormat Bold), EndOfInput
        ]
        doc @?= [Paragraph [
          FormatAtom Bold [WordAtom "world"]
        ]]

      it "format text \"''hello'' '''world'''\"" do
        doc <- wikiText [
          AmbigiousDelimiter (DeFormat Italic), Word "hello",
          AmbigiousDelimiter (DeFormat Italic),
          AmbigiousDelimiter (DeFormat Bold), Word "world",
          AmbigiousDelimiter (DeFormat Bold), EndOfInput
        ]
        doc @?= [Paragraph [
          FormatAtom Italic [WordAtom "hello"],
          FormatAtom Bold [WordAtom "world"]
        ]]

      it "format text \"''hello''\\s'''world'''\"" do
        doc <- wikiText [
          AmbigiousDelimiter (DeFormat Italic), Word "hello",
          AmbigiousDelimiter (DeFormat Italic), Space,
          AmbigiousDelimiter (DeFormat Bold), Word "world",
          AmbigiousDelimiter (DeFormat Bold), EndOfInput
        ]
        doc @?= [Paragraph [
          FormatAtom Italic [WordAtom "hello"],
          FormatAtom Bold [WordAtom "world"]
        ]]


    describe "paragraph" do
      it "end paragraph after line break" do
        doc <- wikiText [
          Word "hello", Space, Word "world", Linebreak,
          Word "hello", Space, Word "angus", EndOfInput]
        doc @=? [
          Paragraph [WordAtom "hello", WordAtom "world"],
          Paragraph [WordAtom "hello", WordAtom "angus"]
        ]


    describe "heading" do
      it "standalone" do
        doc <- wikiText [
          Linebreak,
          AmbigiousDelimiter (DeHeading 6),
          Word "hello", Space, Word "world",
          AmbigiousDelimiter (DeHeading 6), 
          EndOfInput
        ]
        doc @?= [Heading 6 [
          WordAtom "hello", WordAtom "world"
        ]]
        

