{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Test.GraphQL.Lexer
-- Description : Tests for lexing GraphQL atoms
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Test.GraphQL.Lexer where
--------------------------------------------------------------------------------
import           GraphQL.Lexer
--------------------------------------------------------------------------------
import           Test.Hspec
import           Test.QuickCheck
import qualified Data.Text.Encoding as T
import qualified Data.Text          as T
import           Data.ByteString    (ByteString)
--------------------------------------------------------------------------------
lexerSpec :: Spec
lexerSpec = do
  floatSpec
  intSpec
  boolSpec
  nullSpec
  punctuatorSpec
  operationSpec
  directiveSpec
  buildingSpec
  commentSpec

toBs :: Show a => a -> ByteString
toBs = T.encodeUtf8 . T.pack . show

floatSpec :: Spec
floatSpec =
  describe "Should lex floating point numbers" $ do
    it "should lex a Float" $ property $ \x ->
      getTokens (toBs x) `shouldBe` [TokenFloat x]
    it "should lex float with capital 'E' exponent" $ do
      getTokens "-12.34E56" `shouldBe` [TokenFloat (-12.34E56) ]
      getTokens "12.34E56" `shouldBe` [TokenFloat 12.34E56 ]

intSpec :: Spec
intSpec =
  describe "Should lex an Integer value" $ do
    it "should lex an Int" $ property $ \x  ->
      getTokens (toBs x) `shouldBe` [TokenInt x]
    it "should lex an Integer" $ property $ \(x::Integer) ->
      getTokens (toBs x) `shouldBe` [TokenInt (fromIntegral x)]

punctuatorSpec :: Spec
punctuatorSpec =
  describe "Should lex Punctuators" $
    it "should lex all valid punctuation" $
      getTokens "!$()...:=@[]{|}&"
        `shouldBe`
        [ TokenPunctuator '!'
        , TokenPunctuator '$'
        , TokenPunctuator '('
        , TokenPunctuator ')'
        , TokenMultiPunctuator "..."
        , TokenPunctuator ':'
        , TokenPunctuator '='
        , TokenPunctuator '@'
        , TokenPunctuator '['
        , TokenPunctuator ']'
        , TokenPunctuator '{'
        , TokenPunctuator '|'
        , TokenPunctuator '}'
        , TokenPunctuator '&'
        ]

boolSpec :: Spec
boolSpec =
  describe "Should lex a Boolean value" $ do
    it "should lex true" $
      getTokens "true" `shouldBe` [TokenBool True]
    it "should lex false" $
      getTokens "false" `shouldBe` [TokenBool False]

nullSpec :: Spec
nullSpec =
  describe "Should lex a Null Value" $
    it "should lex null" $
      getTokens "null" `shouldBe` [TokenNull]

operationSpec :: Spec
operationSpec =
  describe "Should lex all operation types" $ do
    it "should lex a query" $
      getTokens "query" `shouldBe`
        [TokenOperator Query]
    it "should lex a mutation" $
      getTokens "mutation" `shouldBe`
        [TokenOperator Mutation]
    it "should lex a subscription" $
      getTokens "subscription" `shouldBe`
        [TokenOperator Subscription]

directiveSpec :: Spec
directiveSpec =
  describe "Should lex DirectiveLocations" $ do
    it "should lex an ExecutableDirectiveLocation" $ do
      getTokens "QUERY" `shouldBe`
        [TokenExecutableDirectiveLocation QUERY]
      getTokens "SUBSCRIPTION" `shouldBe`
        [TokenExecutableDirectiveLocation SUBSCRIPTION]
      getTokens "MUTATION" `shouldBe`
        [TokenExecutableDirectiveLocation MUTATION]
      getTokens "FIELD" `shouldBe`
        [TokenExecutableDirectiveLocation FIELD]
      getTokens "FRAGMENT_DEFINITION" `shouldBe`
        [TokenExecutableDirectiveLocation FRAGMENT_DEFINITION]
      getTokens "FRAGMENT_SPREAD" `shouldBe`
        [TokenExecutableDirectiveLocation FRAGMENT_SPREAD]
      getTokens "INLINE_FRAGMENT" `shouldBe`
        [TokenExecutableDirectiveLocation INLINE_FRAGMENT]
    it "should lex an TypeSystemDirectiveLocation" $ do
      getTokens "SCHEMA" `shouldBe`
        [TokenTypeSystemDirectiveLocation SCHEMA]
      getTokens "SCALAR" `shouldBe`
        [TokenTypeSystemDirectiveLocation SCALAR]
      getTokens "OBJECT" `shouldBe`
        [TokenTypeSystemDirectiveLocation OBJECT]
      getTokens "FIELD_DEFINITION" `shouldBe`
        [TokenTypeSystemDirectiveLocation FIELD_DEFINITION]
      getTokens "ARGUMENT_DEFINITION" `shouldBe`
        [TokenTypeSystemDirectiveLocation ARGUMENT_DEFINITION]
      getTokens "INTERFACE" `shouldBe`
        [TokenTypeSystemDirectiveLocation INTERFACE]
      getTokens "UNION" `shouldBe`
        [TokenTypeSystemDirectiveLocation UNION]
      getTokens "ENUM" `shouldBe`
        [TokenTypeSystemDirectiveLocation ENUM]
      getTokens "ENUM_VALUE" `shouldBe`
        [TokenTypeSystemDirectiveLocation ENUM_VALUE]
      getTokens "INPUT_OBJECT" `shouldBe`
        [TokenTypeSystemDirectiveLocation INPUT_OBJECT]
      getTokens "INPUT_FIELD_DEFINITION" `shouldBe`
        [TokenTypeSystemDirectiveLocation INPUT_FIELD_DEFINITION]


buildingSpec :: Spec
buildingSpec =
  describe "Should lex an example query" $
    it "should lex a basic query" $
      getTokens "{building{floorCount}}"
        `shouldBe`
        [ TokenPunctuator '{'
        , TokenName "building"
        , TokenPunctuator '{'
        , TokenName "floorCount"
        , TokenPunctuator '}'
        , TokenPunctuator '}'
        ]

commentSpec :: Spec
commentSpec =
  describe "Should lex an example query w/ comments" $
    it "should lex a basic query" $
      getTokens "{building #some build existings\n { #this is floorcount\n floorCount} #yay a floor count\n }"
        `shouldBe`
        [ TokenPunctuator '{'
        , TokenName "building"
        , TokenPunctuator '{'
        , TokenName "floorCount"
        , TokenPunctuator '}'
        , TokenPunctuator '}'
        ]
