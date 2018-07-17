{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
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
    query
  -- , schema

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
import           Language.Haskell.TH.Syntax hiding (Name)
import           Data.Text.Internal
--------------------------------------------------------------------------------
import           GraphQL.AST
import           GraphQL.Lexer
import           GraphQL.Generic
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
    { quoteExp  = fail "quotExp: not implemented"
    , quotePat  = fail "quotPat: not implemented"
    , quoteDec  = fail "quotDec: not implemented"
    , quoteType = fail "quotType: not implemented"
    }

parseGQLQuery
  :: String
  -> Q Exp
parseGQLQuery
  = either fail liftDataWithText . exeDef
  where
    liftDataWithText :: ExecutableDefinition -> Q Exp
    liftDataWithText edef = do
      scopeTable  <- makeTable (getVariables edef)
      fieldsTable <- makeTable (getFields edef)
      dataToExpQ (withText `extQ` goExeDef scopeTable fieldsTable) edef

    extQ f g a = maybe (f a) g (cast a)
    withText a = liftText <$> cast a

    goExeDef scopeTable fieldsTable (DefinitionOperation opDef) = do
      newDef <- goOpDef scopeTable fieldsTable opDef
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
      let newName = subMaybeName maybeName
      newVars <- traverse (goVarDefs scopeTable) vars
      newDirs <- traverse (subDirs scopeTable) dirs
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      Just [| OperationDefinition opType $newName $(listE newVars) $(listE newDirs) $(listE newSels) |]

    goGType gtype =
      case gtype of
        TypeNamed (NamedType (Name n)) ->
          Just [| TypeNamed $ NamedType $ Name $(litE $ stringL n) |]
        TypeList (ListType anotherGType) ->
          goGType anotherGType
        TypeNonNull nonNullType ->
          case nonNullType of
            NonNullTypeNamed (NamedType (Name n)) ->
              Just [| TypeNamed $ NamedType $ Name $(litE $ stringL n) |]
            NonNullTypeList (ListType anotherGType) ->
              goGType anotherGType

    goVarDefs scopeTable (VariableDefinition (Variable (Name n)) gtype defVal) = do
      newGType <- goGType gtype
      newVar <- Just [| Name $(litE $ stringL n) |]
      newVal <- goDefVal scopeTable defVal
      Just [| VariableDefinition (Variable $newVar) $newGType $newVal |]

    goDefVal _ Nothing = Just [| Nothing |]
    goDefVal scopeTable (Just (DefaultValue val)) = do
      newVal <- subVars scopeTable val
      Just [| Just (DefaultValue $newVal) |]

    subVars scopeTable (ValueVariable (Variable (Name k))) =
      case M.lookup k scopeTable of
        Just True -> Just [| toExpr $(pure $ VarE (mkName k)) |]
        _ -> Just [| toExpr ($(litE $ stringL k) :: String) |]

    subVars scopeTable (ValueString k) =
      case M.lookup k scopeTable of
        Just True -> Just [| toExpr $(pure $ VarE (mkName k)) |]
        _ -> Just [| toExpr ($(litE $ stringL k) :: String) |]

    subVars scopeTable (ValueInt i) = Just [| ValueInt i |]
    subVars scopeTable (ValueFloat f) = Just [| ValueFloat f |]
    subVars scopeTable (ValueBoolean b) = Just [| ValueBoolean b |]
    subVars scopeTable ValueNull = Just [| ValueNull |]
    subVars scopeTable (ValueEnum (EnumValue (Name n))) =
      Just [| ValueEnum $ EnumValue $ Name $(litE $ stringL n) |]
    subVars scopeTable (ValueList vs) = do
      newVals <- traverse (subVars scopeTable) vs
      Just [| ValueList $(listE newVals) |]
    subVars scopeTable (ValueObject os) = do
      newObjs <- traverse (goObjectField scopeTable) os
      Just [| ValueObject $ ObjectValue $(listE newObjs) |]

    goObjectField scopeTable (ObjectField (Name x) v) = do
      newVar <- subVars scopeTable v
      Just [| ObjectField (Name $(litE $ stringL x)) $newVar |]

    subArgs scopeTable (Argument (Name n) var) = do
      newVar <- join $ subVars scopeTable <$> Just var
      Just [| Argument (Name $(litE $ stringL n)) $newVar |]

    subMaybeName (Just (Name n)) = [| Just $ Name ($(litE $ stringL n)) |]
    subMaybeName Nothing = [| Nothing |]

    subMaybeAlias (Just (Alias (Name n))) = [| Just $ Alias $ Name ($(litE $ stringL n)) |]
    subMaybeAlias Nothing = [| Nothing |]

    subName (Name n) = [| Name $(litE $ stringL n) |]

    subDirs scopeTable (Directive name args) = do
      let newName = subName name
      newArgs <- traverse (subArgs scopeTable) args
      Just [| Directive $newName $(listE newArgs) |]

    subFields scopeTable fieldsTable (SelectionField (Field maybeAlias (Name n) args dirs sels)) =
      case M.lookup n fieldsTable of
        Just True -> do
          newSels <- traverse (subFields scopeTable fieldsTable) sels
          newArgs <- traverse (subArgs scopeTable) args
          newDirs <- traverse (subDirs scopeTable) dirs
          let newAlias = subMaybeAlias maybeAlias
          Just [| toField $(pure $ VarE (mkName n))
                   $newAlias
                   $(listE newArgs)
                   $(listE newDirs)
                   $(listE newSels)
                |]
        _ -> do
          newSels <- traverse (subFields scopeTable fieldsTable) sels
          newArgs <- traverse (subArgs scopeTable) args
          newDirs <- traverse (subDirs scopeTable) dirs
          let newAlias = subMaybeAlias maybeAlias
          Just [| toField ($(litE $ stringL n) :: String)
                   $newAlias
                   $(listE newArgs)
                   $(listE newDirs)
                   $(listE newSels)
                |]
    subFields scopeTable fieldsTable (SelectionFragmentSpread (FragmentSpread (Name x) dirs)) = do
      newDirs <- traverse (subDirs scopeTable) dirs
      Just [| SelectionFragmentSpread
                (FragmentSpread
                   (Name $(litE $ stringL x))
                   $(listE newDirs)) |]
    subFields scopeTable fieldsTable (SelectionInlineFragment (InlineFragment maybeTypeCond dirs sels)) = do
      newDirs <- traverse (subDirs scopeTable) dirs
      newSels <- traverse (subFields scopeTable fieldsTable) sels
      let newTypeCond = case maybeTypeCond of
            Nothing -> [| Nothing |]
            Just (TypeCondition (NamedType (Name c))) ->
              [| Just $ NamedType $ Name $(litE $ stringL c) |]
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
    SelectionField $ Field maybeAlias (newName s) args dirs sels
      where
        newName = Name . T.unpack

instance ToField String where
  toField s maybeAlias args dirs sels =
    SelectionField $ Field maybeAlias (Name s) args dirs sels

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
  toExpr = ValueString . T.unpack

instance ToExpr a => ToExpr [a] where
  toExpr = ValueList . map toExpr

instance ToExpr a => ToExpr (Map String a) where
  toExpr x =
    ValueObject $ do
      (k,v) <- M.toList x
      let name = Name k
      pure $ ObjectField name (toExpr v)

instance ToExpr a => ToExpr (Maybe a) where
  toExpr (Just x) = toExpr x
  toExpr Nothing = ValueNull

instance (ToExpr b, ToExpr a) => ToExpr (Either a b) where
  toExpr (Left x) = toExpr x
  toExpr (Right x) = toExpr x

instance {-# overlaps #-} ToExpr String where
  toExpr = ValueString

getVariables
  :: ExecutableDefinition
  -> Set String
getVariables = goDef
  where
    goDef (DefinitionOperation opDef) = goOpDef opDef
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
  -> Set String
getFields = go
  where
    go (DefinitionOperation (AnonymousQuery sels))
      = S.unions (goSelSet <$> sels)
    go (DefinitionOperation (OperationDefinition _ _ _ _ sels))
      = S.unions (goSelSet <$> sels)
    goSelSet (SelectionField (Field _ (Name n) _ _ sels))
      = S.unions (goSelSet <$> sels) <> S.singleton n

instance Lift ExecutableDefinition
instance Lift OperationType
instance Lift TypeCondition
instance Lift Selection
instance Lift Directive
instance Lift Argument
instance Lift Name
instance Lift T.Text
instance Lift Value
