{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE TemplateHaskell      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.QQ
-- Description : Compile-time facilities for dealing with GraphQL schema / queries
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.QQ
  ( schema
  , query
  ) where

import           Data.Attoparsec.Text
import           Data.Data
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           GraphQL.Internal.Name
import           GraphQL.Internal.Syntax.AST
import           GraphQL.Internal.Syntax.Parser
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax

def :: QuasiQuoter
def = QuasiQuoter
    { quoteExp = undefined
    , quotePat = undefined
    , quoteDec = undefined
    , quoteType = undefined
    }

parseGQL
  :: Data a
  => Parser a
  -> String
  -> Q Exp
parseGQL parser
  = either fail liftDataWithText
  . parseOnly parser
  . T.pack
  where
    liftDataWithText :: Data a => a -> Q Exp
    liftDataWithText = dataToExpQ (\a -> liftText <$> cast a)

    liftText :: T.Text -> Q Exp
    liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

schema :: QuasiQuoter
schema
  = def
  { quoteExp = parseGQL schemaDocument
  }

query :: QuasiQuoter
query
  = def
  { quoteExp = parseGQL queryDocument
  }

instance Lift QueryDocument
instance Lift SchemaDocument
deriving instance Data SchemaDocument
deriving instance Data TypeDefinition
deriving instance Data ObjectTypeDefinition
deriving instance Data InterfaceTypeDefinition
deriving instance Data FieldDefinition
deriving instance Data UnionTypeDefinition
deriving instance Data ScalarTypeDefinition
deriving instance Data EnumTypeDefinition
deriving instance Data InputObjectTypeDefinition
deriving instance Data TypeExtensionDefinition
deriving instance Data InputValueDefinition
deriving instance Data EnumValueDefinition
deriving instance Data QueryDocument
deriving instance Data Definition
deriving instance Data FragmentDefinition
deriving instance Data Selection
deriving instance Data Directive
deriving instance Data TypeCondition
deriving instance Data GraphQL.Internal.Name.Name
deriving instance Data OperationDefinition
deriving instance Data VariableDefinition
deriving instance Data Node
deriving instance Data Value
deriving instance Data GType
deriving instance Data Field
deriving instance Data FragmentSpread
deriving instance Data Argument
deriving instance Data InlineFragment
deriving instance Data Variable
deriving instance Data StringValue
deriving instance Data ListValue
deriving instance Data ListType
deriving instance Data ObjectValue
deriving instance Data NonNullType
deriving instance Data ObjectField
