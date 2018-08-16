{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
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

import           Data.Char
import           Control.DeepSeq
import           Control.Monad.State
import           Data.Data
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Read           as T
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
  | TokenString StringValue
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
  | BadEscape Char
  | NoMatch Text
  | UntermBlockString
  | UntermString
  deriving (Show, Eq, Data, Read, Generic, Typeable)

instance NFData Error

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

data StringValue
  = StringValue StringType T.Text
  deriving (Show, Eq, Data, Read, Generic, Typeable)

instance NFData StringValue

token :: (ByteString -> Token) -> Action
token f inputLength _ = do
  LexerState {..} <- get
  pure . pure $ f (B.take inputLength (alexStr matchedInput))

token_ :: Token -> Action
token_ = token . const

data StringType
  = SingleLine
  | BlockString
  deriving (Show, Eq, Data, Read, Generic, Typeable, NFData)

data LexerMode
  = InNormal
  | InBlockString
  | InString
  deriving (Show, Eq)

processEscapedCharacter :: Action
processEscapedCharacter = appendMode

processEscapedUnicode :: Action
processEscapedUnicode len bs =
 case T.hexadecimal $ T.drop 2 $ T.decodeUtf8 (B.take len (alexStr bs)) of
    Right (n, _)
      | n < 0x110000 -> emitChar (chr n) len bs
      | otherwise    -> pure . pure $ TokenError (BadEscape (chr n))
    Left e -> pure . pure $ TokenError (LexerError $ T.pack e)


emitChar :: Char -> Action
emitChar c _ _  = do
  s@LexerState {..} <- get
  put s { stringBuffer = stringBuffer `B8.snoc` c }
  pure Nothing

appendMode :: Action
appendMode = action

action :: Action
action len bs = do
  s@LexerState {..} <- get
  put s { stringBuffer = stringBuffer `B.append` B.take len (alexStr bs) }
  pure Nothing

appendModeBlock :: Action
appendModeBlock = action

endMode :: Action
endMode _ _ = do
  mode <- gets lexerMode
  case mode of
    InNormal -> pure Nothing
    InBlockString -> apply BlockString
    InString -> apply SingleLine
  where
    apply stringType = do
      buf <- gets stringBuffer
      modify $ \s -> s { lexerMode = InNormal, stringBuffer = mempty }
      pure $ Just $ TokenString $ StringValue stringType (T.decodeUtf8 buf)

eofAction :: State LexerState [Token]
eofAction = do
  mode <- gets lexerMode
  pure $ case mode of
    InBlockString -> [TokenError UntermBlockString]
    InString      -> [TokenError UntermString]
    InNormal      -> []

errorAction :: AlexInput -> State LexerState [Token]
errorAction AlexInput {..} =
  pure [TokenError (NoMatch (T.decodeUtf8 alexStr))]

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
