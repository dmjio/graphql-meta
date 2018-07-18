{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.Lexer
-- Description : Lexer / Tokenizer of GraphQL document per GraphQL specification
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.Lexer
  ( getTokens
  , Token (..)
  , Error (..)
  , OperationType (..)
  , ExecutableDirectiveLocation (..)
  , TypeSystemDirectiveLocation (..)
  ) where
--------------------------------------------------------------------------------
import           Data.Char     (chr)
import           Control.Monad
import           Data.Bool
import           Text.Read
import           Data.Data
import           GHC.Generics
import           Data.Typeable
import           Data.Text (Text, pack)
--------------------------------------------------------------------------------
}

%wrapper "basic"

$negativeSign = \-
$zero = 0
$digit = [0-9]
$nonZeroDigit = $digit # 0
$sign = [\+\-]

@integerPart
  = $negativeSign? $zero
  | $negativeSign? $nonZeroDigit ($digit+)?

@intToken
  = @integerPart

@nullToken
  = "null"

@boolToken
  = "true" | "false"

@operationToken
  = query | mutation | subscription

@fractionalPart
  = \. $digit+

@exponentPart
  = [eE] $sign? $digit+

@floatToken
  = @integerPart @fractionalPart
  | @integerPart @exponentPart
  | @integerPart @fractionalPart @exponentPart

@escapedUnicode = [0-9A-Fa-f]{4}
@escapedCharacter = [\"\/\\\b\f\n\r\t]
$sourceCharacter = [\x0009\x000A\x000D\x0020-\xFFFF]
@byteOrderMark = \xFEFE
@whitespace = \x0009 | \x0020
@newLine = \x000A
@carriageReturn = \x000D
@notNewLine = . # \n
@lineTerminator
  = @newLine
  | @carriageReturn @newLine
  | @carriageReturn @notNewLine

@commentChar = $sourceCharacter # \x000A # \x000D \x000A
@comment = \# (@commentChar+)?
$comma = \,
@punctuator =
 \! | \$ | \( | \) | \.{3} | \: | \= | \@ | \[ | \] | \{ | \| | \} | \&
@name = [_A-Za-z][_0-9A-Za-z]*
@escapedCharacter
  = [\"\\\/\b\f\n\r\t]

@escapedUnicode
  = [0-9A-Fa-f]{4}

@stringCharacter
  = $sourceCharacter # \" # \\
  | \\u @escapedUnicode
  | \\ @escapedCharacter

@stringToken
  = \"\"\" ($sourceCharacter+)? \"\"\"
  | \" (@stringCharacter+)? \"

@ignored
  = @byteOrderMark
  | @whitespace
  | @lineTerminator
  | @comment
  | $comma

@executableDirectiveLocationToken
  = "QUERY"
  | "MUTATION"
  | "SUBSCRIPTION"
  | "FIELD"
  | "FRAGMENT_DEFINITION"
  | "FRAGMENT_SPREAD"
  | "INLINE_FRAGMENT"

@typeSystemDirectiveLocationToken
  = "SCHEMA"
  | "SCALAR"
  | "OBJECT"
  | "FIELD_DEFINITION"
  | "ARGUMENT_DEFINITION"
  | "INTERFACE"
  | "UNION"
  | "ENUM"
  | "ENUM_VALUE"
  | "INPUT_OBJECT"
  | "INPUT_FIELD_DEFINITION"

@reserved
  = "input"
  | "implements"
  | "extend"
  | "fragment"
  | "type"
  | "enum"
  | "on"
  | "directive"
  | "scalar"
  | "schema"

tokens :-
  @ignored ;
  @intToken { \s ->
    maybe (TokenError $ ConversionError "Not a valid int" s) TokenInt (readMaybe s) }
  @floatToken { \s ->
    maybe (TokenError $ ConversionError "Not a valid float" s) TokenFloat (readMaybe s) }
  @stringToken { TokenString . pack . processString }
  @reserved { TokenReserved }
  @punctuator { TokenPunctuator }
  @nullToken { \s -> case s of
    "null" -> TokenNull
    otherwise ->
      TokenError (ConversionError "Not a null value" s)
  }
  @boolToken { \s ->
    case s of
      "true" -> TokenBool True
      "false" -> TokenBool False
      otherwise ->
        TokenError (ConversionError "Invalid boolean value" s)
  }
  @operationToken { \s ->
    case s of
      "query" -> TokenOperator Query
      "mutation" -> TokenOperator Mutation
      "subscription" -> TokenOperator Subscription
      otherwise ->
        TokenError (ConversionError "Invalid operator (query, mutation, subscription)" s)
  }
  @executableDirectiveLocationToken { \s ->
    case readMaybe s :: Maybe ExecutableDirectiveLocation of
      Just r -> TokenExecutableDirectiveLocation r
      Nothing -> TokenError (ConversionError "Invalid ExecutableDirectiveLocation" s)
  }
  @typeSystemDirectiveLocationToken { \s ->
    case readMaybe s :: Maybe TypeSystemDirectiveLocation of
      Just r -> TokenTypeSystemDirectiveLocation r
      Nothing -> TokenError (ConversionError "Invalid TypeSystemDirectiveLocation" s)
  }
  @name { TokenName . pack }
{
-- | A GraphQL 'Operation' type
data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Data, Read, Generic, Typeable, Enum)

-- | http://facebook.github.io/graphql/draft/#ExecutableDirectiveLocation
data ExecutableDirectiveLocation
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT
  deriving (Show, Eq, Read, Data, Generic, Typeable, Enum)

-- | http://facebook.github.io/graphql/draft/#TypeSystemDirectiveLocation
data TypeSystemDirectiveLocation
  = SCHEMA
  | SCALAR
  | OBJECT
  | FIELD_DEFINITION
  | ARGUMENT_DEFINITION
  | INTERFACE
  | UNION
  | ENUM
  | ENUM_VALUE
  | INPUT_OBJECT
  | INPUT_FIELD_DEFINITION
  deriving (Show, Eq, Read, Data, Generic, Typeable, Enum)

-- | Token unit for lexing the GraphQL specification
data Token
  = TokenInt Int
  | TokenFloat Double
  | TokenName Text
  | TokenString Text
  | TokenReserved String
  | TokenPunctuator String
  | TokenBool Bool
  | TokenError Error
  | TokenOperator OperationType
  | TokenExecutableDirectiveLocation ExecutableDirectiveLocation
  | TokenTypeSystemDirectiveLocation TypeSystemDirectiveLocation
  | TokenNull
  deriving (Show, Eq, Data, Read, Generic, Typeable)

data Error
  = ConversionError String String
  deriving (Show, Eq, Data, Read, Generic, Typeable)

-- | Retrieve GraphQL tokens
getTokens :: String -> [Token]
getTokens = alexScanTokens

-- | String sanitizing
processString :: String -> String
processString xs = reverse . drop 1 . reverse . drop 1 $ xs

}
