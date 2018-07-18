{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
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
import Test.GraphQL.Lexer  (lexerSpec)
import Test.GraphQL.Parser (parserSpec)
import Test.GraphQL.Gen    (genDocument)
import GraphQL.Pretty      (printDocument)
import GraphQL.Parser      (parseDocument)
--------------------------------------------------------------------------------
import Test.Hspec
import Test.QuickCheck
--------------------------------------------------------------------------------
main :: IO ()
main = hspec (lexerSpec >> parserSpec) >> roundTrip
  where
    roundTrip :: IO ()
    roundTrip = do
      quickCheckWith stdArgs { maxSize = 100000, maxSuccess = 100000 } $ do
        forAll genDocument $ \doc ->
           parseDocument (show (printDocument doc)) === Right doc
