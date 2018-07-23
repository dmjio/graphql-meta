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
import           Data.Bits
import           Data.Char
import           Data.Word
import           GHC.Generics
import qualified Data.Text as T
import           Data.Text (Text)
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
  | extend
  | fragment
  | type
  | enum
  | on
  | directive
  | scalar
  | schema

tokens :-
  @ignored ;
  @intToken { \s ->
    maybe (TokenError $ ConversionError "Not a valid int" s) TokenInt (readMaybe (T.unpack s)) }
  @floatToken { \s ->
    maybe (TokenError $ ConversionError "Not a valid float" s) TokenFloat (readMaybe (T.unpack s)) }
  @stringToken { TokenString . processString }
  "..." { TokenMultiPunctuator }
  @reserved { TokenReserved }
  @punctuator { TokenPunctuator . T.head }
  @nullToken { \s -> case s of
    "null" -> TokenNull
    _ -> TokenError (ConversionError "Not a null value" s)
  }
  @boolToken { \s ->
    case s of
      "true" -> TokenBool True
      "false" -> TokenBool False
      _ ->
        TokenError (ConversionError "Invalid boolean value" s)
  }
  @operationToken { \s ->
    case s of
      "query" -> TokenOperator Query
      "mutation" -> TokenOperator Mutation
      "subscription" -> TokenOperator Subscription
      _ ->
        TokenError (ConversionError "Invalid operator (query, mutation, subscription)" s)
  }
  @executableDirectiveLocationToken { \s ->
    case readMaybe (T.unpack s) :: Maybe ExecutableDirectiveLocation of
      Just r -> TokenExecutableDirectiveLocation r
      Nothing -> TokenError (ConversionError "Invalid ExecutableDirectiveLocation" s)
  }
  @typeSystemDirectiveLocationToken { \s ->
    case readMaybe (T.unpack s) :: Maybe TypeSystemDirectiveLocation of
      Just r -> TokenTypeSystemDirectiveLocation r
      Nothing -> TokenError (ConversionError "Invalid TypeSystemDirectiveLocation" s)
  }
  @name { TokenName }
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
getTokens :: Text -> [Token]
getTokens txt = go (AlexInput '\n' [] txt) 0
  where
    go :: AlexInput -> Int -> [Token]
    go input code =
      case alexScan input code of
        AlexEOF -> []
        AlexError inp -> [TokenError (LexerError (currInput inp))]
        AlexSkip inp _ -> go inp code
        AlexToken inp len action ->
          action (T.take len (currInput input)) : go inp code

data AlexInput = AlexInput
    { prevChar  :: Char
    , currBytes :: [Word8]
    , currInput :: Text
    }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = prevChar

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (AlexInput c bytes text) = case bytes of
    b:ytes -> Just (b, AlexInput c ytes text)
    []     -> case T.uncons text of
        Nothing       -> Nothing
        Just (t, ext) -> case encode t of
            (b, ytes) -> Just (b, AlexInput t ytes ext)

encode :: Char -> (Word8, [Word8])
encode c = (fromIntegral h, map fromIntegral t)
  where
    (h, t) = go (ord c)

    go n
        | n <= 0x7f   = (n, [])
        | n <= 0x7ff  = (0xc0 + (n `shiftR` 6), [0x80 + n .&. 0x3f])
        | n <= 0xffff =
            (   0xe0 + (n `shiftR` 12)
            ,   [   0x80 + ((n `shiftR` 6) .&. 0x3f)
                ,   0x80 + n .&. 0x3f
                ]
            )
        | otherwise   =
            (   0xf0 + (n `shiftR` 18)
            ,   [   0x80 + ((n `shiftR` 12) .&. 0x3f)
                ,   0x80 + ((n `shiftR` 6) .&. 0x3f)
                ,   0x80 + n .&. 0x3f
                ]
            )

utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f = [oc]
   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6), 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12), 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

-- | String sanitizing
processString :: Text -> Text
processString = T.reverse . T.drop 1 . T.reverse . T.drop 1 }
