module Data.WikiText.Tokens (

  Xml(..), 
  WikiToken(..),
  Punctuation(..),
  Delimiter(..),
  AmbigiousDelimiter(..)

) where

import Data.WikiText.Header
import Data.WikiText.TextFormat
import qualified Data.Map as Map


data Xml
  = Opening String (Map.Map String String)
  | Closing String
  | SelfClosing String (Map.Map String String)


data WikiToken
  = Linebreak
  | Space
  | Pipe
  | Xml Xml
  | Word String
  | Punctuation Punctuation
  | OpeningDelimiter Delimiter
  | ClosingDelimiter Delimiter
  | AmbigiousDelimiter AmbigiousDelimiter
  | NamedParameterAssignment
  | Ambigious [WikiToken]
  | EndOfInput


data Punctuation 
  = PPeroid
  | PExclaim
  | PQuestion
  | PComma


data Delimiter
  = DeLink
  | DeXLink
  | DeTemp
  | DeTempPar


data AmbigiousDelimiter
  = DeFormat TextFormat
  | DeHeading HeaderSize


--
-- Eq
--


instance eqWikiToken :: Eq WikiToken where
  (/=) a b = not (a == b)

  (==) (Punctuation p1) (Punctuation p2) = p2 == p2
  (==) (AmbigiousDelimiter a) (AmbigiousDelimiter b) = a == b
  (==) (ClosingDelimiter a) (ClosingDelimiter b) = a == b
  (==) (OpeningDelimiter a) (OpeningDelimiter b) = a == b
  (==) (Word w1) (Word w2) = w1 == w2
  (==) Linebreak Linebreak = true
  (==) Space Space = true
  (==) NamedParameterAssignment NamedParameterAssignment = true
  (==) (Ambigious a) (Ambigious b) = a == b
  (==) (Xml a) (Xml b) = a == b
  (==) EndOfInput EndOfInput = true
  (==) _ _ = false
  

instance eqPunctuation :: Eq Punctuation where
  (/=) a b = not (a == b)

  (==) PPeroid PPeroid = true
  (==) PExclaim PExclaim = true
  (==) PComma PComma = true
  (==) PQuestion PQuestion = true
  (==) _ _ = false


instance eqDelimiter :: Eq Delimiter where
  (/=) a b = not (a == b)
  
  (==) DeLink DeLink = true
  (==) DeXLink DeXLink = true
  (==) DeTemp DeTemp = true
  (==) DeTempPar DeTempPar = true
  (==) _ _ = false


instance eqAmbigiousDelimiter :: Eq AmbigiousDelimiter where
  (/=) a b = not (a == b)
  
  (==) (DeFormat a) (DeFormat b) = a == b
  (==) (DeHeading a) (DeHeading b) = a == b
  (==) _ _ = false


instance eqXml :: Eq Xml where
  (/=) a b = not (a == b)

  (==) (Opening a ma) (Opening b mb) = a == b && ma == mb
  (==) (Closing a) (Closing b) = a == b
  (==) (SelfClosing a ma) (SelfClosing b mb) = a == b && ma == mb
  (==) _ _ = false



--
-- Show
--


instance showWikiToken :: Show WikiToken where
  show (Word w) = "Word " ++ show w
  show Linebreak = "Linebreak"
  show (Punctuation p) = "Punctuation " ++ show p
  show (OpeningDelimiter d) = "OpeningDelimiter (" ++ show d ++ ")"
  show (ClosingDelimiter d) = "ClosingDelimiter (" ++ show d ++ ")"
  show (AmbigiousDelimiter d) = "AmbigiousDelimiter (" ++ show d ++ ")"
  show NamedParameterAssignment = "NamedParameterAssignment"
  show (Ambigious as) = "Ambigious " ++ show as 
  show (Xml t) = "Xml (" ++ show t ++ ")" 
  show EndOfInput = "EndOfInput"
  show Space = "Space"


instance showDelimiter :: Show Delimiter where
  show DeLink = "DeLink"
  show DeXLink = "DeXLink"
  show DeTemp = "DeTemp"
  show DeTempPar = "DeTempPar"
  show DeLink = "DeLink"


instance showAmbigiousDelimiter :: Show AmbigiousDelimiter where
  show (DeFormat f) = "DeFormat (" ++ show f ++ ")"
  show (DeHeading h) = "DeHeading (" ++ show h ++ ")"


instance showPunctuation :: Show Punctuation where
  show PPeroid = "PPeroid" 
  show PExclaim = "PExclaim"
  show PComma = "PComma"
  show PQuestion = "PQuestion"


instance showXml :: Show Xml where
  show (Closing n) = "Closing " ++ show n
  show (Opening n m) = "Opening " ++ show n ++ " " ++ show m
  show (SelfClosing n m) = "SelfClosing " ++ show n ++ " " ++ show m


