{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.Pretty
-- Description : Pretty-printing for the GraphQL AST
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.Pretty where -- ( printExecutableDefinition ) where
--------------------------------------------------------------------------------
import Data.Text.Prettyprint.Doc
--------------------------------------------------------------------------------
import GraphQL.AST
import GraphQL.Lexer
--------------------------------------------------------------------------------
-- | Pretty prints a 'Document'
printDocument
  :: Document
  -> Doc ann
printDocument (Document defs)
  = hsep (printDefinition <$> defs)

-- | Pretty prints a 'Definition'
printDefinition
  :: Definition
  -> Doc ann
printDefinition
  (DefinitionExecutable executableDefinition)
    = printExecutableDefinition executableDefinition
printDefinition
  (DefinitionTypeSystem typeSystemDefinition)
    = printTypeSystemDefinition typeSystemDefinition
printDefinition
  (ExtensionTypeSystem typeSystemExtension)
    = printTypeSystemExtension typeSystemExtension

printTypeSystemExtension
  :: TypeSystemExtension
  -> Doc ann
printTypeSystemExtension (ExtensionSchema schemaExtension)
  = printSchemaExtension schemaExtension
printTypeSystemExtension (ExtensionType typeExtension)
  = printTypeExtension typeExtension

printTypeExtension :: TypeExtension -> Doc a
printTypeExtension (ExtensionScalarType scalarTypeExtension)
  = printScalarTypeExtension scalarTypeExtension
printTypeExtension (ExtensionObjectType objectTypeExtension)
  = printObjectTypeExtension objectTypeExtension
printTypeExtension (ExtensionInterfaceType interfaceTypeExtension)
  = printInterfaceTypeExtension interfaceTypeExtension
printTypeExtension (ExtensionUnionType unionTypeExtension)
  = printUnionTypeExtension unionTypeExtension
printTypeExtension (ExtensionEnumType enumTypeExtension)
  = printEnumTypeExtension enumTypeExtension
printTypeExtension (ExtensionInputObjectType inputObjectTypeExtension)
  = printInputObjectTypeExtension inputObjectTypeExtension

printInputObjectTypeExtension :: InputObjectTypeExtension -> Doc ann
printInputObjectTypeExtension
  (InputObjectTypeExtension (Name name) dirs (InputFieldsDefinition inputFieldsDefinition))
    = mconcat [ pretty name
              , printDirectives dirs
              , foldMap printInputValueDefinition inputFieldsDefinition
              ]

printInputFieldDefinition :: InputValueDefinition -> Doc ann
printInputFieldDefinition
  (InputValueDefinition maybeDescription
                         (Name name)
                         typ
                         maybeDefaultValue
                         dirs)
  = mconcat [ maybe mempty printDescription maybeDescription
            , pretty name
            , printType typ
            , printDirectives dirs
            , printMaybeDefaultValue maybeDefaultValue
            ]

printEnumTypeExtension :: EnumTypeExtension -> Doc ann
printEnumTypeExtension (EnumTypeExtension (Name name) dirs enumValuesDefinition)
  = mconcat [ pretty name
            , printDirectives dirs
            , printEnumValuesDefinition enumValuesDefinition
            ]

printEnumValuesDefinition :: EnumValuesDefinition -> Doc ann
printEnumValuesDefinition (EnumValuesDefinition enumValues) =
  foldMap printEnumValueDefinition enumValues

printEnumValueDefinition :: EnumValueDefinition -> Doc ann
printEnumValueDefinition (EnumValueDefinition maybeDescription enumValue directives)
  = mconcat [ maybe mempty printDescription maybeDescription
            , printEnumValue enumValue
            , printDirectives directives
            ]

printEnumValue
  :: EnumValue -> Doc a
printEnumValue (EnumValue (Name n)) = pretty n

printUnionTypeExtension :: UnionTypeExtension -> Doc ann
printUnionTypeExtension (UnionTypeExtension (Name name) directives unionMemberTypes)
  = mconcat [ pretty name, printDirectives directives
            , printUnionMemberTypes unionMemberTypes
            ]

printUnionMemberTypes :: UnionMemberTypes -> Doc a
printUnionMemberTypes (UnionMemberTypes namedTypes) =
  foldMap printNamedType namedTypes

printNamedType :: NamedType -> Doc a
printNamedType (NamedType (Name n)) = pretty n

printScalarTypeExtension :: ScalarTypeExtension -> Doc ann
printScalarTypeExtension (ScalarTypeExtension (Name name) directives)
  = mconcat [ pretty name
            , printDirectives directives
            ]

printInterfaceTypeExtension :: InterfaceTypeExtension -> Doc ann
printInterfaceTypeExtension
  (InterfaceTypeExtension (Name name) directives fields)
  = mconcat [ pretty name
            , printDirectives directives
            , printFieldsDefinition fields
            ]

printFieldsDefinition :: FieldsDefinition -> Doc ann
printFieldsDefinition (FieldsDefinition fields) =
  foldMap printFieldDefinition fields

printFieldDefinition :: FieldDefinition -> Doc ann
printFieldDefinition
  (FieldDefinition maybeDescription
                    (Name name)
                    (ArgumentsDefinition inputValueDefs)
                    type'
                    directives)
  = hsep [
    maybe mempty printDescription maybeDescription
  , pretty name
  , foldMap printInputValueDefinition inputValueDefs
  , printType type'
  , printDirectives directives
  ]

printObjectTypeExtension :: ObjectTypeExtension -> Doc ann
printObjectTypeExtension
  (ObjectTypeExtension
     (Name name)
     (ImplementsInterfaces is)
     directives
     (FieldsDefinition fields)
  )
  = hsep [
    pretty name
  , foldMap printNamedType is
  , foldMap printFieldDefinition fields
  , printDirectives directives
  ]

printSchemaExtension :: SchemaExtension -> Doc a
printSchemaExtension (SchemaExtension dirs operationTypeDefs) =
  mconcat [ printDirectives dirs
          , vsep (printOperationTypeDefinitions operationTypeDefs)
          ]

printTypeSystemDefinition
  :: TypeSystemDefinition
  -> Doc ann
printTypeSystemDefinition (DefinitionSchema schemaDef) =
  printSchemaDefinition schemaDef
printTypeSystemDefinition (DefinitionType typeDef) =
  printTypeDefinition typeDef
printTypeSystemDefinition (DefinitionDirective dirDef) =
  printDirectiveDefinition dirDef

printSchemaDefinition :: SchemaDefinition -> Doc ann
printSchemaDefinition (SchemaDefinition dirs operationTypeDefs) =
  pretty ("schema" :: String) <>
    printDirectives dirs <+> encloseSep lbrace rbrace comma ops
      where
        ops = printOperationTypeDefinitions operationTypeDefs

printOperationTypeDefinitions
  :: OperationTypeDefinitions -> [Doc ann]
printOperationTypeDefinitions opDefs =
      [ hsep [ printOperationType operationType
             , pretty ':'
             , pretty name
             ]
       | OperationTypeDefinition
           operationType
           namedType <- opDefs
       , let NamedType (Name name) = namedType
       ]

printDirectiveDefinition :: DirectiveDefinition -> Doc ann
printDirectiveDefinition
  (DirectiveDefinition maybeDescription (Name name) argsDefs dirLocs)
    = hsep
      [ maybe mempty printDescription maybeDescription
      , pretty ("directive" :: String)
      , pretty '@'
      , pretty name
      , printArgumentDefinitions argsDefs
      , pretty ("on" :: String)
      , printDirectiveLocations dirLocs
      ]

printArgumentDefinitions :: ArgumentsDefinition -> Doc a
printArgumentDefinitions (ArgumentsDefinition []) = mempty
printArgumentDefinitions (ArgumentsDefinition inputValueDefs)
  = encloseSep lparen rparen comma
      (printInputValueDefinition <$> inputValueDefs)

printInputValueDefinition :: InputValueDefinition -> Doc a
printInputValueDefinition
 (InputValueDefinition
    maybeDescription
    (Name name)
    typ
    maybeDefaultValue
    directives)
  = mconcat [
      maybe mempty (\x -> printDescription x <> space) maybeDescription
    , pretty name
    , space
    , pretty ':'
    , space
    , printType typ
    , space
    , case maybeDefaultValue of
        Nothing -> mempty
        Just x -> printMaybeDefaultValue (Just x) <> space
    , printDirectives directives
    ]

printDescription :: Description -> Doc ann
printDescription (Description s) = pretty '"' <> pretty s <> pretty '"'

printTypeDefinition :: TypeDefinition -> Doc a
printTypeDefinition (DefinitionScalarType scalarTypeDef) =
  printScalarTypeDefinition scalarTypeDef
printTypeDefinition (DefinitionObjectType objectTypeDefinition) =
  printObjectTypeDefinition objectTypeDefinition
printTypeDefinition (DefinitionInterfaceType interfaceTypeDefinition) =
  printInterfaceTypeDefinition interfaceTypeDefinition
printTypeDefinition (DefinitionUnionType unionTypeDefinition) =
  printUnionTypeDefinition unionTypeDefinition
printTypeDefinition (DefinitionEnumType enumTypeDefinition) =
  printEnumTypeDefinition enumTypeDefinition
printTypeDefinition (DefinitionInputObjectType inputObjectTypeDefinition) =
  printInputObjectTypeDefinition inputObjectTypeDefinition

printObjectTypeDefinition :: ObjectTypeDefinition -> Doc ann
printObjectTypeDefinition
  (ObjectTypeDefinition maybeDescription (Name name) implementsInterfaces dirs fieldDefinitions)
  = mconcat [
      maybe mempty (\x -> printDescription x <> space) maybeDescription
    , pretty ("type" :: String)
    , space
    , pretty name
    , space
    , printImplementsInterfaces implementsInterfaces
    , printDirectives dirs
    , printFieldsDefinitions fieldDefinitions
    ]

printImplementsInterfaces
  :: ImplementsInterfaces
  -> Doc a
printImplementsInterfaces
  (ImplementsInterfaces []) = mempty
printImplementsInterfaces
  (ImplementsInterfaces namedTypes)
  = pretty ("implements" :: String) <> space <>
  mconcat [ pretty '&' <> space <> pretty name <> space
          | (NamedType (Name name)) <- namedTypes
          ]

printFieldsDefinitions :: FieldsDefinition -> Doc a
printFieldsDefinitions (FieldsDefinition []) = mempty
printFieldsDefinitions (FieldsDefinition fields) =
  pretty '{' <+> hsep (go <$> fields) <+> pretty '}'
    where
      go (FieldDefinition maybeDescription (Name name) argsDefs type' directives)
        = mconcat [ maybe mempty (\x -> printDescription x <> space) maybeDescription
                  , pretty name
                  , space
                  , printArgumentDefinitions argsDefs
                  , space
                  , pretty ':'
                  , space
                  , printType type'
                  , space
                  , printDirectives directives
                  ]

printInterfaceTypeDefinition :: InterfaceTypeDefinition -> Doc a
printInterfaceTypeDefinition
  (InterfaceTypeDefinition maybeDescription (Name name) directives fields)
   = mconcat [
      maybe mempty (\x -> printDescription x <> space) maybeDescription
    , pretty ("interface" :: String)
    , space
    , pretty name
    , printDirectives directives
    , printFieldsDefinition fields
    ]

printUnionTypeDefinition :: UnionTypeDefinition -> Doc a
printUnionTypeDefinition
  (UnionTypeDefinition maybeDescription
                        (Name name)
                        directives
                        _)
  = mconcat [
      maybe mempty printDescription maybeDescription
    , pretty name
    , printDirectives directives
    ]

printEnumTypeDefinition :: EnumTypeDefinition -> Doc a
printEnumTypeDefinition
  (EnumTypeDefinition maybeDescription
                       (Name name)
                       directives
                       _)
  = mconcat [
      maybe mempty printDescription maybeDescription
    , pretty name
    , printDirectives directives
    ]

printInputObjectTypeDefinition :: InputObjectTypeDefinition -> Doc a
printInputObjectTypeDefinition
  (InputObjectTypeDefinition maybeDescription
                       (Name name)
                       directives
                       _)
  = mconcat [
      maybe mempty printDescription maybeDescription
    , pretty name
    , printDirectives directives
    ]

printScalarTypeDefinition :: ScalarTypeDefinition -> Doc ann
printScalarTypeDefinition
  (ScalarTypeDefinition maybeDescription (Name name) directives)
    = mconcat [
        maybe mempty (\s -> printDescription s <> space) maybeDescription
      , pretty ("scalar" :: String)
      , space
      , pretty name
      , printDirectives directives
      ]

printDirectiveLocations :: DirectiveLocations -> Doc a
printDirectiveLocations xs = hsep (printDirectiveLocation <$> xs)
  where
    printDirectiveLocation (LocationExecutableDirective l)
      = pretty (show l)
    printDirectiveLocation (LocationTypeSystemDirective l)
      = pretty (show l)

-- | Pretty prints an 'ExecutableDefinition'
printExecutableDefinition
  :: ExecutableDefinition
  -> Doc ann
printExecutableDefinition
  (DefinitionOperation operationDefinition)
    = printOperationDefinition operationDefinition
printExecutableDefinition
  (DefinitionFragment fragmentDefinition)
    = printFragmentDefinition fragmentDefinition

printOperationDefinition
  :: OperationDefinition
  -> Doc ann
printOperationDefinition (AnonymousQuery selectionSet)
  = printSelectionSet selectionSet
printOperationDefinition
  (OperationDefinition operationType maybeName varDefs dirs selSet)
  = mconcat [
      printOperationType operationType
    , mconcat [ space <> pretty name | Just (Name name) <- pure maybeName ]
    , printVariableDefinitions varDefs
    , printDirectives dirs
    , printSelectionSet selSet
    ]

printVariableDefinitions :: [VariableDefinition] -> Doc ann
printVariableDefinitions varDefs
  | null varDefs = mempty
  | otherwise =
     encloseSep lparen rparen comma (printVariableDefinition <$> varDefs)

printVariableDefinition :: VariableDefinition -> Doc ann
printVariableDefinition (VariableDefinition variable typ' maybeDefaultValue)
  = printVariable variable
  <> colon
  <+> printType typ'
  <+> printMaybeDefaultValue maybeDefaultValue

printMaybeDefaultValue :: Maybe DefaultValue -> Doc ann
printMaybeDefaultValue (Just (DefaultValue val))  = pretty '=' <+> printValue val
printMaybeDefaultValue Nothing = mempty

printType :: Type -> Doc ann
printType (TypeNamed (NamedType (Name n))) =
  pretty n
printType (TypeList (ListType typ)) =
  lbracket <> printType typ <> rbracket
printType (TypeNonNull (NonNullTypeList (ListType typ))) =
  lbracket <> printType typ <> rbracket <> pretty '!'
printType (TypeNonNull (NonNullTypeNamed (NamedType (Name n)))) =
  pretty n <> pretty '!'

printOperationType
  :: OperationType
  -> Doc ann
printOperationType Query = pretty ("query" :: String)
printOperationType Mutation = pretty ("mutation" :: String)
printOperationType Subscription = pretty ("subscription" :: String)

printSelectionSet
  :: [Selection]
  -> Doc ann
printSelectionSet selSet =
  mconcat [
     space
   , nest 2 $ vsep [
       lbrace
     , vsep (printSel `fmap` selSet)
     , rbrace
   ]
 ]

printMaybeSelectionSet
  :: [Selection]
  -> Doc ann
printMaybeSelectionSet [] = mempty
printMaybeSelectionSet selSet =
  mconcat [
     space
   , nest 2 $ vsep [
       lbrace
     , vsep (printSel `fmap` selSet)
     , rbrace
   ]
 ]


printSel
  :: Selection
  -> Doc ann
printSel (SelectionField field)
  = space <+> printField field
printSel (SelectionFragmentSpread fragmentSpread)
  = printFragmentSpread fragmentSpread
printSel (SelectionInlineFragment inlineFragment)
  = printInlineFragment inlineFragment

printInlineFragment :: InlineFragment -> Doc ann
printInlineFragment (InlineFragment (Just typeCondition) dirs selSet)
  = pretty ("..." :: String)
      <+> printTypeCondition typeCondition
      <> printDirectives dirs
      <> printSelectionSet selSet

printInlineFragment (InlineFragment Nothing [] [])
  = hsep [
      pretty ("..." :: String)
    , lbrace
    , rbrace
    ]

printInlineFragment (InlineFragment Nothing dirs selSet)
  = pretty ("..." :: String)
      <> printDirectives dirs
      <> printSelectionSet selSet

printFragmentSpread :: FragmentSpread -> Doc ann
printFragmentSpread (FragmentSpread (Name n) dirs)
  = pretty ("..." :: String)
      <+> pretty n
      <> printDirectives dirs

printField :: Field -> Doc ann
printField (Field maybeAlias (Name name) args dirs sels)
  = mconcat
    [ hsep [ space <> pretty alias <> colon
           | Just (Alias (Name alias)) <- pure maybeAlias
           ]
    , space
    , pretty name
    , printArgs args
    , printDirectives dirs
    , printMaybeSelectionSet sels
    ]

printArgs :: Arguments -> Doc ann
printArgs args
  | null args = mempty
  | otherwise =
      encloseSep lparen rparen comma (map printArg args)

printArg :: Argument -> Doc ann
printArg (Argument (Name name) value) =
  pretty name <> colon <> printValue value

printValue :: Value -> Doc ann
printValue (ValueString s)      = pretty '"' <> pretty s <> pretty '"'
printValue (ValueBoolean True)  = pretty ("true" :: String)
printValue (ValueBoolean False) = pretty ("false" :: String)
printValue ValueNull            = pretty ("null" :: String)
printValue (ValueInt int)       = pretty int
printValue (ValueEnum ev)       = printEnum ev
printValue (ValueFloat float)   = pretty float
printValue (ValueVariable var)  = printVariable var
printValue (ValueList values)   =
  encloseSep lbracket rbracket comma (map printValue values)
printValue (ValueObject values) =
  encloseSep lbrace rbrace comma (map printObjectField values)

printObjectField :: ObjectField -> Doc ann
printObjectField (ObjectField (Name name) val)
  = pretty name <> colon <> printValue val

printVariable :: Variable -> Doc ann
printVariable (Variable (Name n))    = pretty '$' <> pretty n

printEnum :: EnumValue -> Doc ann
printEnum (EnumValue (Name n))  = pretty n

printDirectives :: Directives -> Doc ann
printDirectives [] = mempty
printDirectives xs = space <> hsep (map printDirective xs)

printDirective :: Directive -> Doc ann
printDirective (Directive (Name n) args)
  = pretty '@' <> pretty n <> printArgs args

printFragmentDefinition :: FragmentDefinition -> Doc ann
printFragmentDefinition
  (FragmentDefinition (Name n) typeCondition directives selectionSet)
  = pretty ("fragment" :: String)
      <+> pretty n
      <+> printTypeCondition typeCondition
      <> printDirectives directives
      <> printSelectionSet selectionSet

printTypeCondition :: TypeCondition -> Doc ann
printTypeCondition (TypeCondition (NamedType (Name n)))
  = pretty ("on" :: String) <+> pretty n

-- lok  = print $ getTokens $ show $ printExecutableDefinition k
-- shok = print $ printExecutableDefinition k
-- pok  = exeDef $ show $ printExecutableDefinition k
-- rok = Right k == pok
