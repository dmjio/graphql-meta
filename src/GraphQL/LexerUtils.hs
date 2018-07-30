{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.LexerUtils
-- Description : Lexer / Tokenizer of GraphQL document per GraphQL specification
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.LexerUtils where

import           Control.DeepSeq
import           Control.Monad.State
import           Data.Data
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import           Data.Word
import           GHC.Generics
import           Text.Read                hiding (get)

import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as B8
import qualified Data.ByteString.Internal as B (w2c)

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
  | NoMatch Char
  | UntermBlockString
  | UntermString
  deriving (Show, Eq, Data, Read, Generic, Typeable)

instance NFData Error

--(AlexInput '\n' str 0)

data AlexInput = AlexInput
  { alexChar    :: {-# UNPACK #-} !Char
  , alexStr     :: {-# UNPACK #-} !B.ByteString
  , alexBytePos :: {-# UNPACK #-} !Int
  } deriving (Show, Eq)

data LexerState
  = LexerState
  { matchedInput :: !AlexInput
  , lexerMode    :: !LexerMode
  , stringBuffer :: !ByteString
  } deriving (Show, Eq)

type Action = Int -> AlexInput -> State LexerState (Maybe Token)

token :: (ByteString -> Token) -> Action
token f inputLength _ = do
  LexerState {..} <- get
  pure . pure $ f (B.take inputLength (alexStr matchedInput))

token_ :: Token -> Action
token_ = token . const

data LexerMode
  = InNormal
  | InBlockString
  | InString
  deriving (Show, Eq)

processEscapedCharacter :: Action
processEscapedCharacter = appendMode -- token (TokenString . T.decodeUtf8)

processEscapedUnicode :: Action
processEscapedUnicode = appendMode -- token (TokenString . T.decodeUtf8)

appendMode :: Action
appendMode _ (B.uncons . alexStr -> Just (c, _)) = do
  s@LexerState {..} <- get
  put s { stringBuffer = B.cons c stringBuffer }
  pure Nothing
appendMode _ _ =
  pure Nothing

endMode :: Action
endMode _ _ = do
  mode <- gets lexerMode
  case mode of
    InNormal -> pure Nothing
    InBlockString -> apply
    InString -> apply
  where
    apply = do
      modify $ \s -> s { lexerMode = InNormal }
      buf <- gets stringBuffer
      pure $ Just $ TokenString (T.decodeUtf8 buf)

eofAction :: State LexerState [Token]
eofAction = do
  mode <- gets lexerMode
  pure $ case mode of
    InBlockString -> [TokenError UntermBlockString]
    InString      -> [TokenError UntermString]
    InNormal      -> []

errorAction :: AlexInput -> State LexerState [Token]
errorAction AlexInput {..} =
  pure [TokenError (NoMatch (T.head $ T.decodeUtf8 alexStr))]

startBlockString :: Action
startBlockString _ _ =
  Nothing <$ do
    modify $ \s -> s { lexerMode = InBlockString }

startString :: Action
startString _ _ =
  Nothing <$ do
    modify $ \s -> s { lexerMode = InString }

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar = alexChar

alexGetByte :: AlexInput -> Maybe (Word8, AlexInput)
alexGetByte AlexInput {..} =
  case B.uncons alexStr of
    Nothing -> Nothing
    Just (c, rest) ->
      Just (c, AlexInput {
        alexChar = B.w2c c,
        alexStr = rest,
        alexBytePos = alexBytePos+1
      })

parseIntToken :: ByteString -> Token
parseIntToken s =
  maybe (TokenError $ ConversionError "Not a valid int" (T.decodeUtf8 s)) TokenInt (readMaybe (T.unpack $ T.decodeUtf8 s))

parseFloatToken :: ByteString -> Token
parseFloatToken s =
  maybe (TokenError $ ConversionError "Not a valid float" (T.decodeUtf8 s)) TokenFloat (readMaybe (B8.unpack s))

parseExecutableDirectiveLocationToken :: ByteString -> Token
parseExecutableDirectiveLocationToken s =
  case readMaybe (T.unpack $ T.decodeUtf8 s) :: Maybe ExecutableDirectiveLocation of
    Just r -> TokenExecutableDirectiveLocation r
    Nothing -> TokenError (ConversionError "Invalid ExecutableDirectiveLocation" (T.decodeUtf8 s))

parseTypeSystemDirectiveLocationToken :: ByteString -> Token
parseTypeSystemDirectiveLocationToken s =
  case readMaybe (T.unpack $ T.decodeUtf8 s) :: Maybe TypeSystemDirectiveLocation of
    Just r -> TokenTypeSystemDirectiveLocation r
    Nothing -> TokenError (ConversionError "Invalid TypeSystemDirectiveLocation" (T.decodeUtf8 s))
