{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RecordWildCards    #-}
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
  , StringValue (..)
  , StringType (..)
  ) where
--------------------------------------------------------------------------------
import           Data.ByteString (ByteString)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Control.Monad.State
--------------------------------------------------------------------------------
import           GraphQL.LexerUtils
--------------------------------------------------------------------------------
}

$comma = \,
$digit = [0-9]
$negativeSign = \-
$nonZeroDigit = $digit # 0
$sign = [\+\-]
$sourceCharacter = [\x0009\x000A\x000D\x0020-\xFFFF]
$zero = 0

@integerPart
  = $negativeSign? $zero
  | $negativeSign? $nonZeroDigit $digit*

@intToken
  = @integerPart

@fractionalPart
  = \. $digit+

@exponentPart
  = [eE] $sign? $digit+

@floatToken
  = @integerPart @fractionalPart
  | @integerPart @exponentPart
  | @integerPart @fractionalPart @exponentPart

@escapedUnicode = [0-9A-Fa-f]{4}
@byteOrderMark = \xFEFE
@whitespace = \x0009 | \x0020
@lineTerminator = \x000A | \x000D \x000A | \x000D .

@comment = "#" ($sourceCharacter # \n # [\r\n])*
@punctuator = [\!\$\(\)\:\=\@\[\]\{\|\}\&]
@name = [_A-Za-z][_0-9A-Za-z]*
@escapedCharacter
  = [\"\\\/bfnrt]

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
  \"\"\" { startBlockString }
  \" { startString }
  @ignored ;
  @intToken { token parseIntToken }
  @floatToken { token parseFloatToken }
  "..." { token (TokenMultiPunctuator . T.decodeUtf8) }
  @reserved { token (TokenReserved . T.decodeUtf8) }
  @punctuator { token (TokenPunctuator . T.head . T.decodeUtf8) }
  "null" { token_ TokenNull }
  "true" { token_ (TokenBool True) }
  "false" { token_ (TokenBool False) }
  "query" { token_ (TokenOperator Query) }
  "mutation" { token_ (TokenOperator Mutation) }
  "subscription" { token_ (TokenOperator Subscription) }
  @executableDirectiveLocationToken { token parseExecutableDirectiveLocationToken }
  @typeSystemDirectiveLocationToken { token parseTypeSystemDirectiveLocationToken }
  @name { token (TokenName . T.decodeUtf8) }
  }
 <blockstring> {
  [.\n] { appendModeBlock }
  \\\"\"\" { endMode }
  \"\"\" { endMode }
 }
 <string> {
   \\ b { emitChar '\b' }
   \\ t { emitChar '\t' }
   \\ n { emitChar '\n' }
   \\ f { emitChar '\f' }
   \\ r { emitChar '\r' }
   \\ \" { emitChar '"' }
   \\ \\ { emitChar '\\' }
   \\ u @escapedUnicode { processEscapedUnicode }
   $sourceCharacter # [\"\n\\] { appendMode }
   \" { endMode }
 }

{
getTokens :: ByteString -> [Token]
getTokens = alexScanTokens

mkInput :: ByteString -> AlexInput
mkInput s = AlexInput '\n' s 0

alexScanTokens :: ByteString -> [Token]
alexScanTokens input = flip evalState (LexerState (mkInput input) InNormal mempty) go
  where
    go :: State LexerState [Token]
    go = do
      LexerState {..} <- get
      case alexScan matchedInput (stateToInt lexerMode) of
        AlexEOF -> eofAction
        AlexError alexInput -> errorAction alexInput
        AlexSkip alexInput _ -> do
          modify $ \s -> s { matchedInput = alexInput }
          go
        AlexToken alexInput _ act -> do
          let len = alexBytePos alexInput - alexBytePos matchedInput
          r <- act len matchedInput
          modify $ \s -> s { matchedInput = alexInput  }
          case r of
            Nothing -> go
            Just t -> do
              ts <- go
              pure (t:ts)

stateToInt :: LexerMode -> Int
stateToInt InNormal{}      = 0
stateToInt InString{}      = string
stateToInt InBlockString{} = blockstring

}
