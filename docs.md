# Module Documentation

## Module Data.WikiText

#### `WikiText`

``` purescript
data WikiText
  = Heading Number [WikiAtom]
  | Paragraph [WikiAtom]
```


#### `WikiAtom`

``` purescript
data WikiAtom
  = WordAtom String
```


#### `eqWikiText`

``` purescript
instance eqWikiText :: Eq WikiText
```

#### `eqWikiAtom`

``` purescript
instance eqWikiAtom :: Eq WikiAtom
```


#### `showWikiText`

``` purescript
instance showWikiText :: Show WikiText
```

#### `showWikiAtom`

``` purescript
instance showWikiAtom :: Show WikiAtom
```



## Module Data.WikiText.TextFormat

#### `TextFormat`

``` purescript
data TextFormat
  = Italic 
  | Bold 
  | ItalicBold 
```


#### `eqTextFormat`

``` purescript
instance eqTextFormat :: Eq TextFormat
```


#### `showTextFormat`

``` purescript
instance showTextFormat :: Show TextFormat
```



## Module Data.WikiText.Parsing.Errors

#### `ParseError`

``` purescript
data ParseError
  = Intern InternalError
  | String String
  | Unknown 
```


#### `InternalError`

``` purescript
data InternalError
  = RanOutOfTokens 
```


#### `errorParseError`

``` purescript
instance errorParseError :: Error ParseError
```


#### `showParseError`

``` purescript
instance showParseError :: Show ParseError
```


#### `showInternalError`

``` purescript
instance showInternalError :: Show InternalError
```



## Module Data.WikiText.Parsing.Parser

#### `WikiTextParser`

``` purescript
type WikiTextParser a = Parser [WikiToken] a
```


#### `wikiText`

``` purescript
wikiText :: WikiTextParser [WikiText]
```


#### `eqAmbigiousDelimiterType`

``` purescript
instance eqAmbigiousDelimiterType :: Eq AmbigiousDelimiterType
```


#### `eqTokenType`

``` purescript
instance eqTokenType :: Eq TokenType
```



## Module Data.WikiText.Parsing.Tokens

#### `WikiTokenParser`

``` purescript
type WikiTokenParser a = Parser String a
```


#### `tokens`

``` purescript
tokens :: WikiTokenParser [WikiToken]
```




