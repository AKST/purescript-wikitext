module Data.WikiText where


import Data.WikiText.LinkTarget
import Data.WikiText.TextFormat


data WikiText
  = Heading Number [WikiAtom]
  | Paragraph [WikiAtom]


data WikiAtom
  = WordAtom String
  | FormatAtom TextFormat [WikiAtom]
  


-- EQ INSTANCES


instance eqWikiText :: Eq WikiText where
  (/=) l r = not (l == r)
  (==) (Heading a1 b1) (Heading a2 b2) = a1 == a2 && b1 == b2 
  (==) (Paragraph p1) (Paragraph p2) = p1 == p2
  (==) _              _              = false
 
instance eqWikiAtom :: Eq WikiAtom where
  (/=) l r = not (l == r)
  
  (==) (WordAtom a1) (WordAtom a2) = a1 == a2 
  (==) (FormatAtom f1 a1) (FormatAtom f2 a2) = f1 == f2 && a1 == a2 
  (==) _ _ = false


-- SHOW INSTANCES


instance showWikiText :: Show WikiText where
  show (Heading s b) = "WikiText " ++ show s ++ " " ++ show b
  show (Paragraph p) = "Paragraph " ++ show p

instance showWikiAtom :: Show WikiAtom where
  show (WordAtom text) = "WordAtom " ++ show text
  show (FormatAtom f atoms) = "FormatAtom " ++ show f ++ " " ++ show atoms


