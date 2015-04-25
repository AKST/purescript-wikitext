module Data.WikiText.Parsing.TokenTests (tests) where


import Control.Monad.Eff

import Test.Mocha
import Test.Assert.Simple

import Data.Tuple
import qualified Data.Map as Map
import qualified Data.WikiText.Parsing.Tokens as Parser
import Data.WikiText.Header
import Data.WikiText.TextFormat
import Data.WikiText.Tokens

import qualified TestCommon as Test


tokens :: String -> Eff _ [WikiToken]
tokens = Test.parseOrFail Parser.tokens


--
-- Spec:
--   http://www.mediawiki.org/wiki/Markup_spec
--
tests = do
  describe "Text.WikiText.Tokens" do
    describe "whitespace" do
      it " " do
        result <- tokens " " 
        result @?= [Space, EndOfInput]

      it "  " do
        result <- tokens "  "
        result @?= [Space, Space, EndOfInput]

    describe "linebreaks" do
      it "\\n" do
        result <- tokens "\n"
        result @?= [Linebreak, EndOfInput]

    describe "words" do
      it "hello world" do
        result <- tokens "hello world"
        result @?= [Word "hello", Space, Word "world", EndOfInput]

    describe "punctuation" $ do
      it "." do
        result <- tokens "."
        result @?= [Punctuation PPeroid, EndOfInput]

      it "hello, world!" do
        result <- tokens "hello, world!"
        result @?= [
          Word "hello", 
          Punctuation PComma, 
          Space, 
          Word "world", 
          Punctuation PExclaim, 
					EndOfInput
        ]

    describe "delimters" do
      it "'' ''' '''''" do
        result <- tokens "'' ''' '''''"
        result @?= [
          AmbigiousDelimiter (DeFormat Italic),
          Space, 
          AmbigiousDelimiter (DeFormat Bold),
          Space, 
          AmbigiousDelimiter (DeFormat ItalicBold), 
					EndOfInput
        ]

      it "[[[ [[ {{{ {{" do
        result <- tokens "[[[ [[ {{{ {{"
        result @?= [
          OpeningDelimiter DeXLink,
          Space,
          OpeningDelimiter DeLink,
          Space,
          OpeningDelimiter DeTempPar,
          Space,
          OpeningDelimiter DeTemp, 
					EndOfInput
        ]

      it "====== ===== ==== === == =" do
        result <- tokens "====== ===== ==== === == ="
        result @?= [
          AmbigiousDelimiter (DeHeading Size6), 
          Space,
          AmbigiousDelimiter (DeHeading Size5), 
          Space,
          AmbigiousDelimiter (DeHeading Size4), 
          Space,
          AmbigiousDelimiter (DeHeading Size3), 
          Space,
          AmbigiousDelimiter (DeHeading Size2), 
          Space,
          Ambigious [
            AmbigiousDelimiter (DeHeading Size1), 
            NamedParameterAssignment
          ], 
					EndOfInput
        ]

    describe "xml" do
      it "<ref>hello</ref>" do
        result <- tokens "<ref>hello</ref>"
        result @?= [
          Xml (Opening "ref" (Map.fromList [])),
          Word "hello",
          Xml (Closing "ref"), 
					EndOfInput
        ]
        
      it "<ref/>" do
        result <- tokens "<ref/>"
        result @?= [Xml (SelfClosing "ref" (Map.fromList [])), EndOfInput]

      it "<ref />" do
        result <- tokens "<ref />"
        result @?= [Xml (SelfClosing "ref" (Map.fromList [])), EndOfInput]


      it "< ref ></ ref >" do
        result <- tokens "< ref ></ ref >"
        result @?= [
					Xml (Opening "ref" (Map.fromList [])), 
					Xml (Closing "ref"), 
					EndOfInput
				]


      it "<ref name=\"john\"></ref>" do
        result <- tokens "<ref name=\"john\"></ref>"
        result @?= [
          Xml (Opening "ref" (Map.fromList [
            Tuple "name" "john"
          ])),
          Xml (Closing "ref"), 
					EndOfInput
        ]

      it "<ref name=\"john\"></ref>" do
        result <- tokens "<ref name=\"john\"/>"
        result @?= [
					Xml (SelfClosing "ref" (Map.fromList [
					  Tuple "name" "john"
					])), 
				  EndOfInput
				]

