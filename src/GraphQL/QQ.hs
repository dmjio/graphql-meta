{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.QQ
-- Description : Compile-time facilities for dealing with GraphQL schema / queries
-- Maintainer  : David Johnson <code@dmj.io>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.QQ
  ( -- * QuasiQuoters
    query
  -- , schema

    -- * Classes
  , ToExpr  (..)
  , ToField (..)
  ) where
--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Data
import qualified Data.Map                   as M
import           Data.Map                   (Map)
import           Data.Maybe
import           Data.Set                   (Set)
import qualified Data.Set                   as S
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax hiding (Name)
import           Data.Text.Internal
--------------------------------------------------------------------------------
import           GraphQL.AST
import           GraphQL.Lexer
import           GraphQL.Parser
--------------------------------------------------------------------------------

-- | QuasiQuoter for GraphQL 'Schema' definitions
-- schema :: QuasiQuoter
-- schema = def { quoteExp = parseGQLSchema }

-- | Parsing the GraphQL Schema
-- parseGQLSchema
--   :: String
--   -> Q Exp
-- parseGQLSchema
--   = either fail (dataToExpQ (fmap liftText . cast))
--   . parseOnly schemaDocument
--   . T.pack

-- | QuasiQuoter for GraphQL 'QueryDocument' definitions
query :: QuasiQuoter
query = def { quoteExp = parseGQLQuery }

def :: QuasiQuoter
def = QuasiQuoter
    { quoteExp  = error "quotExp: not implemented"
    , quotePat  = error "quotPat: not implemented"
    , quoteDec  = error "quotDec: not implemented"
    , quoteType = error "quotType: not implemented"
    }

parseGQLQuery
  :: String
  -> Q Exp
parseGQLQuery
  = either fail liftDataWithText . exeDef . T.encodeUtf8 . T.pack
  where
    liftDataWithText :: ExecutableDefinition -> Q Exp
    liftDataWithText edef = do
      scopeTable  <- makeTable (getVariables edef)
      fieldsTable <- makeTable (getFields edef)
      dataToExpQ (withText `extQ` goExeDef scopeTable fieldsTable) edef

    extQ f g a = maybe (f a) g (cast a)
    withText a = liftText <$> cast a

    goExeDef scopeTable fieldsTable (DefinitionOperation oDef) = do
      newDef <- goOpDef scopeTable fieldsTable oDef
      Just [| DefinitionOperation $newDef |]

    goExeDef scopeTable fieldsTable (DefinitionFragment fragDef) = do
      newDef <- goFragDef scopeTable fieldsTable fragDef
      Just [| DefinitionFragment $newDef |]

    goFragDef scopeTable fieldsTable (FragmentDefinition name typeCond dirs sels) = do
      newDirs <- traverse (subDirs scopeTable) dirs
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      Just [| DefinitionFragment name typeCond $(listE newDirs) $(listE newSels) |]

    goOpDef scopeTable fieldsTable (AnonymousQuery sels) = do
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      Just [| AnonymousQuery $(listE newSels) |]

    goOpDef scopeTable fieldsTable (OperationDefinition opType maybeName vars dirs sels) = do
      let newName' = subMaybeName maybeName
      newVars <- traverse (goVarDefs scopeTable) vars
      newDirs <- traverse (subDirs scopeTable) dirs
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      Just [| OperationDefinition opType $newName' $(listE newVars) $(listE newDirs) $(listE newSels) |]

    goGType gtype =
      case gtype of
        TypeNamed (NamedType (Name n)) ->
          Just [| TypeNamed $ NamedType $ Name $(litE $ stringL $ T.unpack n) |]
        TypeList (ListType anotherGType) ->
          goGType anotherGType
        TypeNonNull nonNullType ->
          case nonNullType of
            NonNullTypeNamed (NamedType (Name n)) ->
              Just [| TypeNonNull $ NonNullTypeNamed $ NamedType $ Name $(litE $ stringL $ T.unpack n) |]
            NonNullTypeList (ListType anotherGType) ->
              goGType anotherGType

    goVarDefs scopeTable (VariableDefinition (Variable (Name n)) gtype defVal) = do
      newGType <- goGType gtype
      newVar <- Just [| Name $(litE $ stringL $ T.unpack n) |]
      newVal <- goDefVal scopeTable defVal
      Just [| VariableDefinition (Variable $newVar) $newGType $newVal |]

    goDefVal _ Nothing = Just [| Nothing |]
    goDefVal scopeTable (Just (DefaultValue val)) = do
      newVal <- subVars scopeTable val
      Just [| Just (DefaultValue $newVal) |]

    subVars scopeTable (ValueVariable (Variable (Name k))) =
      case M.lookup k scopeTable of
        Just True -> Just [| toExpr $(pure $ VarE (mkName $ T.unpack k)) |]
        _ -> Just [| ValueVariable (Variable (Name $(litE $ stringL $ T.unpack k))) |]

    subVars scopeTable (ValueString (StringValue _ k)) =
      case M.lookup k scopeTable of
        Just True -> Just [| toExpr $(pure $ VarE (mkName $ T.unpack k)) |]
        _ -> Just [| toExpr ($(litE $ stringL $ T.unpack k) :: String) |]

    subVars _ (ValueInt i) = Just [| ValueInt i |]
    subVars _ (ValueFloat f) = Just [| ValueFloat f |]
    subVars _ (ValueBoolean b) = Just [| ValueBoolean b |]
    subVars _ ValueNull = Just [| ValueNull |]
    subVars _ (ValueEnum (EnumValue (Name n))) =
      Just [| ValueEnum $ EnumValue $ Name $(litE $ stringL $ T.unpack n) |]
    subVars scopeTable (ValueList vs) = do
      newVals <- traverse (subVars scopeTable) vs
      Just [| ValueList $(listE newVals) |]
    subVars scopeTable (ValueObject os) = do
      newObjs <- traverse (goObjectField scopeTable) os
      Just [| ValueObject $ ObjectValue $(listE newObjs) |]

    goObjectField scopeTable (ObjectField (Name x) v) = do
      newVar <- subVars scopeTable v
      Just [| ObjectField (Name $(litE $ stringL $ T.unpack x)) $newVar |]

    subArgs scopeTable (Argument (Name n) var) = do
      newVar <- subVars scopeTable =<< Just var
      Just [| Argument (Name $(litE $ stringL $ T.unpack n)) $newVar |]

    subMaybeName (Just (Name n)) = [| Just $ Name $(litE $ stringL $ T.unpack n) |]
    subMaybeName Nothing = [| Nothing |]

    subMaybeAlias (Just (Alias (Name n))) = [| Just $ Alias $ Name $(litE $ stringL $ T.unpack n) |]
    subMaybeAlias Nothing = [| Nothing |]

    subName (Name n) = [| Name $(litE $ stringL $ T.unpack n) |]

    subDirs scopeTable (Directive name args) = do
      let newName' = subName name
      newArgs <- traverse (subArgs scopeTable) args
      Just [| Directive $newName' $(listE newArgs) |]

    subFields scopeTable fieldsTable (SelectionField (Field maybeAlias (Name n) args dirs sels)) = do
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      newArgs <- traverse (subArgs scopeTable) args
      newDirs <- traverse (subDirs scopeTable) dirs
      let newAlias = subMaybeAlias maybeAlias
      case M.lookup n fieldsTable of
        Just True ->
          Just [| toField $(pure $ VarE (mkName $ T.unpack n))
                   $newAlias
                   $(listE newArgs)
                   $(listE newDirs)
                   $(listE newSels)
                |]
        _ ->
          Just [| toField ($(litE $ stringL $ T.unpack n) :: String)
                   $newAlias
                   $(listE newArgs)
                   $(listE newDirs)
                   $(listE newSels)
                |]
    subFields scopeTable _ (SelectionFragmentSpread (FragmentSpread (Name x) dirs)) = do
      newDirs <- traverse (subDirs scopeTable) dirs
      Just [| SelectionFragmentSpread
                (FragmentSpread
                   (Name $(litE $ stringL $ T.unpack x))
                   $(listE newDirs)) |]
    subFields scopeTable fieldsTable (SelectionInlineFragment (InlineFragment maybeTypeCond dirs sels)) = do
      newDirs <- traverse (subDirs scopeTable) dirs
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      let newTypeCond = case maybeTypeCond of
            Nothing -> [| Nothing |]
            Just (TypeCondition (NamedType (Name c))) ->
              [| Just $ NamedType $ Name $(litE $ stringL $ T.unpack c) |]
      Just [| SelectionInlineFragment
                (InlineFragment
                   $newTypeCond
                   $(listE newDirs)
                   $(listE newSels)
                )
            |]

makeTable :: S.Set Text -> Q (Map Text Bool)
makeTable xs = M.unions <$> do
  forM (S.toList xs) $ \x -> do
    result <- lookupValueName (T.unpack x)
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
    SelectionField $ Field maybeAlias (Name s) args dirs sels

instance ToField String where
  toField s maybeAlias args dirs sels =
    SelectionField $ Field maybeAlias (Name $ T.pack s) args dirs sels

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
  toExpr = ValueString . StringValue SingleLine

instance ToExpr a => ToExpr [a] where
  toExpr = ValueList . map toExpr

instance ToExpr a => ToExpr (Map String a) where
  toExpr x =
    ValueObject $ do
      (k,v) <- M.toList x
      let name = Name (T.pack k)
      pure $ ObjectField name (toExpr v)

instance ToExpr a => ToExpr (Maybe a) where
  toExpr (Just x) = toExpr x
  toExpr Nothing = ValueNull

instance (ToExpr b, ToExpr a) => ToExpr (Either a b) where
  toExpr (Left x) = toExpr x
  toExpr (Right x) = toExpr x

instance {-# overlaps #-} ToExpr String where
  toExpr = ValueString . StringValue SingleLine . T.pack

getVariables
  :: ExecutableDefinition
  -> Set Text
getVariables = goDef
  where
    goDef (DefinitionOperation oDef) = goOpDef oDef
    goDef (DefinitionFragment _) = mempty
    goOpDef (OperationDefinition _ _ vars _ selSets) =
      S.unions $ mconcat
          [ goSelSet <$> selSets
          , goVar <$> vars
          ]
    goOpDef (AnonymousQuery selSets)
      = S.unions $ goSelSet <$> selSets
    goSelSet (SelectionField (Field _ _ args _ selSets))
      = S.unions $ mconcat
          [ goSelSet <$> selSets
          , goArg <$> args
          ]
    goSelSet _ = mempty
    goArg (Argument _ (ValueVariable (Variable (Name v))))
      = S.singleton v
    goArg _ = mempty
    goVar (VariableDefinition (Variable (Name v)) _ _) =
      S.singleton v

getFields
  :: ExecutableDefinition
  -> Set Text
getFields = go
  where
    go (DefinitionFragment _) = mempty
    go (DefinitionOperation (AnonymousQuery sels))
      = S.unions (goSelSet <$> sels)
    go (DefinitionOperation (OperationDefinition _ _ _ _ sels))
      = S.unions (goSelSet <$> sels)
    goSelSet (SelectionField (Field _ (Name n) _ _ sels))
      = S.unions (goSelSet <$> sels) <> S.singleton n
    goSelSet _ = mempty

instance Lift ExecutableDefinition
instance Lift OperationType
instance Lift TypeCondition
instance Lift Selection
instance Lift Directive
instance Lift Argument
instance Lift Name
instance Lift Value
