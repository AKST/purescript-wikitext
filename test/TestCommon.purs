module TestCommon where

import Control.Monad.Error.Trans
import Control.Monad.Cont.Trans
import Control.Monad.Eff


import qualified Text.Parsing.Parser as Parser
import qualified Text.Parsing.Parser.Combinators as Parser
import qualified Text.Parsing.Parser.Expr as Parser
import qualified Text.Parsing.Parser.String as Parser
import qualified Text.Parsing.Parser.Token as Parser

import qualified Debug.Trace as Trace
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.String as String


parseOrFail :: forall s a eff. Parser.Parser s a -> s -> Eff eff a 
parseOrFail parser input = case Parser.runParser input parser of
  Either.Right result                 -> pure result
  Either.Left (Parser.ParseError err) -> throwException err.message


shouldFailParse :: forall s a eff. Parser.Parser s a -> s -> Eff eff Parser.ParseError
shouldFailParse parser input = case Parser.runParser input parser of
  Either.Right _  -> throwException "should not have parsed"
  Either.Left err -> pure err



rightOrFail :: forall e a eff. (Show e) => Either.Either e a -> Eff eff a
rightOrFail either = case either of
  Either.Right r  -> pure r
  Either.Left err -> throwException (show err) 


fromMaybe :: forall e a. String -> Maybe.Maybe a -> Eff e a
fromMaybe _ (Maybe.Just a) = pure a
fromMaybe m _              = throwException m


foreign import throwException """
  function throwException (message) {
    return function () {
      throw new Error(message);
    };
  }
  """ :: forall e a. String -> Eff e a


printTitle name = Trace.trace formattedTitle where
  formattedTitle = border ++ "\n" ++ center ++ "\n" ++ border where
    border = repeat "#" testTitleSize 
    center =
      let nameLength = String.length name 
          marginSize = testTitleSize - nameLength  
          lrMargin   = (repeat " " (marginSize / 2))
      in (lrMargin ++ name) 

  repeat s n = repeatIter n "" where
    repeatIter n acc 
      | n <= 0    = acc
      | otherwise = repeatIter (n-1) (s ++ acc)    

  testTitleSize = 80

