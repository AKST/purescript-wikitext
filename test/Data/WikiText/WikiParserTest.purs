module Data.WikiText.Parsing.ParserTests (tests) where

import Control.Monad.Eff

import Data.WikiText
import Data.WikiText.Tokens
import Data.WikiText.TextFormat
import Data.WikiText.LinkTarget
import Data.WikiText.Header
import qualified Data.WikiText.Parsing.Parser as Parser

import qualified Data.Maybe as Maybe

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


      describe "format text" do
        it "\"''hello''\"" do
          doc <- wikiText [
            AmbigiousDelimiter (DeFormat Italic), Word "hello",
            AmbigiousDelimiter (DeFormat Italic), EndOfInput
          ]
          doc @?= [Paragraph [
            FormatAtom Italic [WordAtom "hello"]
          ]]
  
        it "\"'''world'''\"" do
          doc <- wikiText [
            AmbigiousDelimiter (DeFormat Bold), Word "world",
            AmbigiousDelimiter (DeFormat Bold), EndOfInput
          ]
          doc @?= [Paragraph [
            FormatAtom Bold [WordAtom "world"]
          ]]
  
        it "\"''hello'' '''world'''\"" do
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
  
        it "\"''hello''\\s'''world'''\"" do
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


      describe "links" do 
        it "internal link with default label" do
          doc <- wikiText [
            OpeningDelimiter DeLink, Word "hello",
            ClosingDelimiter DeLink, EndOfInput]
          doc @?= [Paragraph [
            HyperTextAtom Internal "hello" Maybe.Nothing
          ]]

        it "external link with default label" do
          doc <- wikiText [
            OpeningDelimiter DeXLink, Word "hello",
            ClosingDelimiter DeXLink, EndOfInput]
          doc @?= [Paragraph [
            HyperTextAtom External "hello" Maybe.Nothing
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
          AmbigiousDelimiter (DeHeading Size6),
          Word "hello", Space, Word "world",
          AmbigiousDelimiter (DeHeading Size6), 
          EndOfInput
        ]
        doc @?= [Heading Size6 [
          WordAtom "hello", WordAtom "world"
        ]]
        

