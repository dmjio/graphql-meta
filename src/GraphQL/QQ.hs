{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.QQ
-- Description : Compile-time facilities for dealing with GraphQL schema / queries
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.QQ
  ( -- * QuasiQuoters
    schema
  , query
    -- * Classes
  , ToExpr (..)
  ) where
--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Attoparsec.Text
import           Data.Data
import qualified Data.Map                       as M
import           Data.Map                       (Map)
import           Data.Maybe
import           Data.Set                       (Set)
import qualified Data.Set                       as S
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
--------------------------------------------------------------------------------
import qualified GraphQL.Internal.Name          as GQLName
import           GraphQL.Internal.Syntax.AST
import           GraphQL.Internal.Syntax.Parser
--------------------------------------------------------------------------------

-- | QuasiQuoter for GraphQL 'Schema' definitions
schema :: QuasiQuoter
schema = def { quoteExp = parseGQLSchema }

-- | QuasiQuoter for GraphQL 'QueryDocument' definitions
query :: QuasiQuoter
query = def { quoteExp = parseGQLQuery }

def :: QuasiQuoter
def = QuasiQuoter
    { quoteExp = fail "quotExp: not implemented"
    , quotePat = fail "quotPat: not implemented"
    , quoteDec = fail "quotDec: not implemented"
    , quoteType = fail "quotType: not implemented"
    }

parseGQLSchema
  :: String
  -> Q Exp
parseGQLSchema
  = either fail (dataToExpQ (\a -> liftText <$> cast a))
  . parseOnly schemaDocument
  . T.pack

parseGQLQuery
  :: String
  -> Q Exp
parseGQLQuery
  = either fail liftDataWithText
  . parseOnly queryDocument
  . T.pack
  where
    liftDataWithText :: QueryDocument -> Q Exp
    liftDataWithText doc = do
      let vars = S.toList (getVariables doc)
      scopeTable <- M.unions <$> do
        flip traverse vars $ \var -> do
          inScope <- isJust <$> lookupValueName var
          pure $ if inScope
            then M.singleton var True
            else mempty
      dataToExpQ (k `extQ` substituteVariables scopeTable) doc
    k a = liftText <$> cast a
    substituteVariables scopeTable (ValueVariable (Variable (GQLName.Name n)))
      | Just True <- M.lookup (T.unpack n) scopeTable =
         Just [| toExpr $(pure $ VarE (mkName $ T.unpack n)) |]
      | otherwise = Nothing

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

extQ
  :: ( Typeable a
     , Typeable b
     )
  => (a -> q)
  -> (b -> q)
  -> a
  -> q
extQ f g a = maybe (f a) g (cast a)

-- | Used to convert Haskell metavariables into GraphQL Values
class ToExpr a where
  toExpr :: a -> Value

instance ToExpr Int where
  toExpr = ValueInt . fromIntegral

instance ToExpr Integer where
  toExpr = ValueInt . fromIntegral

instance ToExpr Bool where
  toExpr = ValueBoolean

instance ToExpr Double where
  toExpr = ValueFloat

instance ToExpr Text where
  toExpr = ValueString . StringValue

instance ToExpr a => ToExpr [a] where
  toExpr = ValueList . ListValue . map toExpr

instance ToExpr a => ToExpr (Map String a) where
  toExpr x =
    ValueObject . ObjectValue $ do
      (k,v) <- M.toList x
      let name = GQLName.Name (T.pack k)
      pure $ ObjectField name (toExpr v)

instance ToExpr a => ToExpr (Maybe a) where
  toExpr (Just x) = toExpr x
  toExpr Nothing = ValueNull

instance (ToExpr b, ToExpr a) => ToExpr (Either a b) where
  toExpr (Left x) = toExpr x
  toExpr (Right x) = toExpr x

instance {-# overlaps #-} ToExpr String where
  toExpr = ValueString . StringValue . T.pack

getVariables
  :: QueryDocument
  -> Set String
getVariables = go
  where
    go (QueryDocument defs)
      = S.unions $ goDef <$> defs
    goDef (DefinitionOperation (Query (Node _ vars _ selSets)))
      = S.unions $ mconcat
          [ goSelSet <$> selSets
          , goVar <$> vars
          ]
    goDef (DefinitionOperation (AnonymousQuery selSets))
      = S.unions $ goSelSet <$> selSets
    goDef _ = mempty
    goSelSet (SelectionField (Field _ _ args _ selSets))
      = S.unions $ mconcat
          [ goSelSet <$> selSets
          , goArg <$> args
          ]
    goSelSet _ = mempty
    goArg (Argument _ (ValueVariable (Variable (GQLName.Name v))))
      = S.singleton (T.unpack v)
    goArg _ = mempty
    goVar (VariableDefinition (Variable (GQLName.Name v)) _ _) =
      S.singleton (T.unpack v)

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
deriving instance Data GQLName.Name
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
