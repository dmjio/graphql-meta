{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE OverloadedStrings  #-}
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
import           Text.Read
import           Data.Data
import           GHC.Generics
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Internal as ByteString (w2c)
import           Control.DeepSeq
--------------------------------------------------------------------------------
}

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
@lineTerminator = \x000A | \x000D \x000A | \x000D .

@comment = "#" ($sourceCharacter # \n)*
$comma = \,
@punctuator = [\!\$\(\)\:\=\@\[\]\{\|\}\&]
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
  = QUERY
  | MUTATION
  | SUBSCRIPTION
  | FIELD
  | FRAGMENT_DEFINITION
  | FRAGMENT_SPREAD
  | INLINE_FRAGMENT

@typeSystemDirectiveLocationToken
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

@reserved
  = input
  | implements
  | interface
  | extend
  | union
  | fragment
  | type
  | enum
  | on
  | directive
  | scalar
  | schema

tokens :-
 <0> {
  @ignored ;
  @intToken { \s ->
    maybe (TokenError $ ConversionError "Not a valid int" (T.decodeUtf8 s)) TokenInt (readMaybe (B8.unpack s)) }
  @floatToken { \s ->
    maybe (TokenError $ ConversionError "Not a valid float" (T.decodeUtf8 s)) TokenFloat (readMaybe (B8.unpack s)) }
  @stringToken { TokenString . processString . T.decodeUtf8 }
  "..." { TokenMultiPunctuator . T.decodeUtf8 }
  @reserved { TokenReserved . T.decodeUtf8 }
  @punctuator { TokenPunctuator . T.head . T.decodeUtf8 }
  @nullToken { \s -> case s of
    "null" -> TokenNull
    _ -> TokenError (ConversionError "Not a null value" (T.decodeUtf8 s))
  }
  @boolToken { \s ->
    case s of
      "true" -> TokenBool True
      "false" -> TokenBool False
      _ ->
        TokenError (ConversionError "Invalid boolean value" (T.decodeUtf8 s))
  }
  @operationToken { \s ->
    case s of
      "query" -> TokenOperator Query
      "mutation" -> TokenOperator Mutation
      "subscription" -> TokenOperator Subscription
      _ ->
        TokenError (ConversionError "Invalid operator (query, mutation, subscription)" (T.decodeUtf8 s))
  }
  @executableDirectiveLocationToken { \s ->
    case readMaybe (B8.unpack s) :: Maybe ExecutableDirectiveLocation of
      Just r -> TokenExecutableDirectiveLocation r
      Nothing -> TokenError (ConversionError "Invalid ExecutableDirectiveLocation" (T.decodeUtf8 s))
  }
  @typeSystemDirectiveLocationToken { \s ->
    case readMaybe (B8.unpack s) :: Maybe TypeSystemDirectiveLocation of
      Just r -> TokenTypeSystemDirectiveLocation r
      Nothing -> TokenError (ConversionError "Invalid TypeSystemDirectiveLocation" (T.decodeUtf8 s))
  }
  @name { TokenName . T.decodeUtf8 }
  }

{

-- | A GraphQL 'Operation' type
data OperationType
  = Query
  | Subscription
  | Mutation
  deriving (Show, Eq, Data, Read, Generic, Typeable, Enum)

instance NFData OperationType

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

instance NFData ExecutableDirectiveLocation

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

instance NFData TypeSystemDirectiveLocation

-- | Token unit for lexing the GraphQL specification
data Token
  = TokenInt Int
  | TokenFloat Double
  | TokenName Text
  | TokenString Text
  | TokenReserved Text
  | TokenPunctuator Char
  | TokenMultiPunctuator Text
  | TokenBool Bool
  | TokenError Error
  | TokenOperator OperationType
  | TokenExecutableDirectiveLocation ExecutableDirectiveLocation
  | TokenTypeSystemDirectiveLocation TypeSystemDirectiveLocation
  | TokenNull
  deriving (Show, Eq, Data, Read, Generic, Typeable)

data Error
  = ConversionError Text Text
  | LexerError Text
  deriving (Show, Eq, Data, Read, Generic, Typeable)

instance NFData Error

-- | Retrieve GraphQL tokens
getTokens = alexScanTokens

alexScanTokens str = go (AlexInput '\n' str 0)
  where
    go inp =
      case alexScan inp 0 of
        AlexEOF -> []
        AlexError e -> [TokenError $ LexerError (T.decodeUtf8 (alexStr e))]
        AlexSkip  inp' _  -> go inp'
        AlexToken inp' _ act -> do
          let len = alexBytePos inp' - alexBytePos inp
          act (ByteString.take len (alexStr inp)) : go inp'

data AlexInput = AlexInput
  { alexChar :: {-# UNPACK #-} !Char
  , alexStr :: {-# UNPACK #-} !ByteString.ByteString
  , alexBytePos :: {-# UNPACK #-} !Int
  }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar

alexGetByte (AlexInput {alexStr=cs,alexBytePos=n}) =
    case ByteString.uncons cs of
        Nothing -> Nothing
        Just (c, rest) ->
            Just (c, AlexInput {
                alexChar = ByteString.w2c c,
                alexStr =  rest,
                alexBytePos = n+1})

processString :: Text -> Text
processString = T.reverse . T.drop 1 . T.reverse . T.drop 1

}
