module Data.WikiText.LinkTarget where


data LinkTarget
  = External
  | Internal

instance showLinkTarget :: Show LinkTarget where
  show External = "External"
  show Internal = "Internal"

instance eqLinkTarget :: Eq LinkTarget where
  (/=) l r = not (l == r)

  (==) External External = true
  (==) Internal Internal = true
  (==) _ _ = false

