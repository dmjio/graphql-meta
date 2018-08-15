{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : GraphQL test runner
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Main where
--------------------------------------------------------------------------------
import qualified Data.Text           as T
import qualified Data.Text.Encoding  as T
--------------------------------------------------------------------------------
import           Test.GraphQL.Lexer  (lexerSpec)
import           Test.GraphQL.Parser (parserSpec)
import           Test.GraphQL.Gen    (genDocument) -- , genValue)
import           GraphQL.Pretty      (printDocument) --, printValue)
import           GraphQL.Parser      (parseDocument) -- , value)
--------------------------------------------------------------------------------
import           Test.Hspec
import           Test.QuickCheck
--------------------------------------------------------------------------------
main :: IO ()
main = hspec (lexerSpec >> parserSpec) >> roundTrip
  where
    roundTrip :: IO ()
    roundTrip =
      quickCheckWith stdArgs { maxSize = 100000, maxSuccess = 100000 } $
        forAll genDocument $ \doc ->
           parseDocument (T.encodeUtf8 . T.pack $ show (printDocument doc)) === Right doc
    -- roundTripVal :: IO ()
    -- roundTripVal =
    --   quickCheckWith stdArgs { maxSize = 100000, maxSuccess = 100000 } $
    --     forAll genValue $ \v ->
    --        value (T.encodeUtf8 . T.pack $ show (printValue v)) === Right v
