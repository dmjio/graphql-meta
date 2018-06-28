{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
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
  , ToExpr  (..)
  , ToField (..)
  ) where
--------------------------------------------------------------------------------
import           Control.Monad
import           Control.Applicative
import           Data.Monoid
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
import           Data.Text.Internal
import           Unsafe.Coerce
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
  = either fail (dataToExpQ (fmap liftText . cast))
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
      scopeTable  <- makeTable (getVariables doc)
      fieldsTable <- makeTable (getFields doc)
      dataToExpQ (withText `extQ` goDoc scopeTable fieldsTable) doc

    extQ f g a = maybe (f a) g (cast a)
    withText a = liftText <$> cast a

    goDoc scopeTable fieldsTable (QueryDocument defs) = do
      newDefs <- traverse (goDefs scopeTable fieldsTable) defs
      Just [| QueryDocument $(listE newDefs) |]

    goDefs scopeTable fieldsTable (DefinitionOperation (Query node)) = do
      newNode <- goNode scopeTable fieldsTable node
      Just [| DefinitionOperation (Query $newNode) |]

    goDefs scopeTable fieldsTable (DefinitionOperation (AnonymousQuery sels)) = do
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      Just [| DefinitionOperation (AnonymousQuery $(listE newSels)) |]

    goGType gtype =
      case gtype of
        TypeNamed (NamedType (GQLName.Name n)) ->
          Just [| TypeNamed $ NamedType $ GQLName.Name $(litE $ stringL $ T.unpack n) |]
        TypeList (ListType anotherGType) ->
          goGType anotherGType
        TypeNonNull nonNullType ->
          case nonNullType of
            NonNullTypeNamed (NamedType (GQLName.Name n)) ->
              Just [| TypeNamed $ NamedType $ GQLName.Name $(litE $ stringL $ T.unpack n) |]
            NonNullTypeList (ListType anotherGType) ->
              goGType anotherGType

    goNode scopeTable fieldsTable (Node maybeName vars dirs sels) = do
      let newName = subMaybeName maybeName
      newVars <- traverse (goVarDefs scopeTable) vars
      newDirs <- traverse (subDirs scopeTable) dirs
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      Just [| Node $newName $(listE newVars) $(listE newDirs) $(listE newSels) |]

    goVarDefs scopeTable (VariableDefinition (Variable (GQLName.Name n)) gtype defVal) = do
      newGType <- goGType gtype
      newVar <- Just [| GQLName.Name $(litE $ stringL $ T.unpack n) |]
      newVal <- goDefVal scopeTable defVal
      Just [| VariableDefinition (Variable $newVar) $newGType $newVal |]

    goDefVal _ Nothing = Just [| Nothing |]
    goDefVal scopeTable (Just val) = do
      newVal <- subVars scopeTable val
      Just [| Just $newVal |]

    subVars scopeTable (ValueVariable (Variable (GQLName.Name k))) =
      case M.lookup (T.unpack k) scopeTable of
        Just True -> Just [| toExpr $(pure $ VarE (mkName $ T.unpack k)) |]
        _ -> Just [| toExpr ($(litE $ stringL $ T.unpack k) :: String) |]

    subVars scopeTable (ValueString (StringValue k)) =
      case M.lookup (T.unpack k) scopeTable of
        Just True -> Just [| toExpr $(pure $ VarE (mkName $ T.unpack k)) |]
        _ -> Just [| toExpr ($(litE $ stringL $ T.unpack k) :: String) |]

    subVars scopeTable (ValueInt i) = Just [| ValueInt i |]
    subVars scopeTable (ValueFloat f) = Just [| ValueFloat f |]
    subVars scopeTable (ValueBoolean b) = Just [| ValueBoolean b |]
    subVars scopeTable ValueNull = Just [| ValueNull |]
    subVars scopeTable (ValueEnum (GQLName.Name n)) =
      Just [| ValueEnum $ GQLName.Name $(litE $ stringL $ T.unpack n) |]
    subVars scopeTable (ValueList (ListValue vs)) = do
      newVals <- traverse (subVars scopeTable) vs
      Just [| ValueList $(listE newVals) |]
    subVars scopeTable (ValueObject (ObjectValue os)) = do
      newObjs <- traverse (goObjectField scopeTable) os
      Just [| ValueObject $ ObjectValue $(listE newObjs) |]

    goObjectField scopeTable (ObjectField (GQLName.Name x) v) = do
      newVar <- subVars scopeTable v
      Just [| ObjectField (GQLName.Name $(litE $ stringL $ T.unpack x)) $newVar |]

    subArgs scopeTable (Argument (GQLName.Name n) var) = do
      newVar <- join $ subVars scopeTable <$> Just var
      Just [| Argument (GQLName.Name $(litE $ stringL $ T.unpack n)) $newVar |]

    subMaybeName (Just (GQLName.Name n)) = [| Just $ GQLName.Name ($(litE $ stringL $ T.unpack n)) |]
    subMaybeName Nothing = [| Nothing |]

    subName (GQLName.Name n) = [| GQLName.Name $(litE $ stringL $ T.unpack n) |]

    subDirs scopeTable (Directive name args) = do
      let newName = subName name
      newArgs <- traverse (subArgs scopeTable) args
      Just [| Directive $newName $(listE newArgs) |]

    subFields scopeTable fieldsTable (SelectionField (Field maybeAlias (GQLName.Name n) args dirs sels)) =
      case M.lookup (T.unpack n) fieldsTable of
        Just True -> do
          newSels <- traverse (subFields scopeTable fieldsTable) sels
          newArgs <- traverse (subArgs scopeTable) args
          newDirs <- traverse (subDirs scopeTable) dirs
          let newAlias = subMaybeName maybeAlias
          Just [| toField $(pure $ VarE (mkName $ T.unpack n))
                   $newAlias
                   $(listE newArgs)
                   $(listE newDirs)
                   $(listE newSels)
                |]
        _ -> do
          newSels <- traverse (subFields scopeTable fieldsTable) sels
          newArgs <- traverse (subArgs scopeTable) args
          newDirs <- traverse (subDirs scopeTable) dirs
          let newAlias = subMaybeName maybeAlias
          Just [| toField ($(litE $ stringL $ T.unpack n) :: String)
                   $newAlias
                   $(listE newArgs)
                   $(listE newDirs)
                   $(listE newSels)
                |]
    subFields scopeTable fieldsTable (SelectionFragmentSpread (FragmentSpread (GQLName.Name x) dirs)) = do
      newDirs <- traverse (subDirs scopeTable) dirs
      Just [| SelectionFragmentSpread
                (FragmentSpread
                   (GQLName.Name $(litE $ stringL $ T.unpack x))
                   $(listE newDirs)) |]
    subFields scopeTable fieldsTable (SelectionInlineFragment (InlineFragment maybeTypeCond dirs sels)) = do
      newDirs <- traverse (subDirs scopeTable) dirs
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      let newTypeCond = case maybeTypeCond of
            Nothing -> [| Nothing |]
            Just (NamedType (GQLName.Name c)) ->
              [| Just $ NamedType $ GQLName.Name $(litE $ stringL $ T.unpack c) |]
      Just [| SelectionInlineFragment
                (InlineFragment
                   $newTypeCond
                   $(listE newDirs)
                   $(listE newSels)
                )
            |]

makeTable :: S.Set String -> Q (Map String Bool)
makeTable xs = M.unions <$> do
  flip traverse (S.toList xs) $ \x -> do
    result <- lookupValueName x
    let inScope = isJust result
    pure $ if inScope
           then M.singleton x True
           else mempty

liftText :: T.Text -> Q Exp
liftText txt = AppE (VarE 'T.pack) <$> lift (T.unpack txt)

-- | Used to convert Haskell metavariables into GraphQL Values
class ToField a where
  toField :: a
          -> Maybe Alias
          -> [Argument]
          -> [Directive]
          -> SelectionSet
          -> Selection

instance ToField Text where
  toField s maybeAlias args dirs sels =
    SelectionField $ Field maybeAlias (GQLName.Name s) args dirs sels

instance ToField String where
  toField s maybeAlias args dirs sels =
    SelectionField $ Field maybeAlias (GQLName.Name $ T.pack s) args dirs sels

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

getFields
  :: QueryDocument
  -> Set String
getFields = go
  where
    go (QueryDocument defs)
      = S.unions $ goDef <$> defs
    goDef (DefinitionOperation (Query (Node _ vars _ selSets)))
      = S.unions (goSelSet <$> selSets)
    goDef (DefinitionOperation (AnonymousQuery selSets))
      = S.unions (goSelSet <$> selSets)
    goDef _ = mempty
    goSelSet (SelectionField (Field _ (GQLName.Name n) _ _ selSets))
      = S.unions (goSelSet <$> selSets) <> S.singleton (T.unpack n)

instance Lift QueryDocument
instance Lift SchemaDocument
instance Lift Selection
instance Lift Directive
instance Lift Argument
instance Lift GQLName.Name
instance Lift T.Text
instance Lift Value
#if !MIN_VERSION_graphql_api(0,3,0)
instance Lift GraphQL.Internal.Syntax.AST.Type
#else
instance Lift GType
#endif
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
#if !MIN_VERSION_graphql_api(0,3,0)
deriving instance Data GraphQL.Internal.Syntax.AST.Type
#else
deriving instance Data GType
#endif
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
