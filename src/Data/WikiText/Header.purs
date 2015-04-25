module Data.WikiText.Header where


data HeaderSize
  = Size1
  | Size2
  | Size3
  | Size4
  | Size5
  | Size6


sizeToNumber :: HeaderSize -> Number
sizeToNumber size = 
  case size of
    Size1 -> 1
    Size2 -> 2
    Size3 -> 3
    Size4 -> 4
    Size5 -> 5
    Size6 -> 6


instance showHeaderSize :: Show HeaderSize where
  show size = "Size" ++ (size # sizeToNumber # show)

instance eqHeaderSize :: Eq HeaderSize where
  (/=) l r = not (l == r)
  (==) l r = sizeToNumber l == sizeToNumber r

