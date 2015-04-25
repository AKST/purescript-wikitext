module AllTests where

import qualified Debug.Trace as Trace

import Control.Monad.Eff

import TestCommon
import qualified Control.Monad.JQuery as J

import qualified Data.WikiText.Parsing.TokenTests as TokenTests
import qualified Data.WikiText.Parsing.ParserTests as ParserTests


main = J.ready do
  initMocha
  TokenTests.tests
  ParserTests.tests
  runMocha



foreign import initMocha """
  var initMocha = mocha.setup.bind(mocha, 'bdd');
  """ :: forall e. Eff e Unit

foreign import runMocha """
  var runMocha = (function () {
    var context = (window.mochaPhantomJS || window.mocha);
    return context.run.bind(context); 
  }());
  """ :: forall e. Eff e Unit

