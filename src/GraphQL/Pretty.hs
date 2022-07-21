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
module GraphQL.Pretty where
--------------------------------------------------------------------------------
import Prettyprinter
--------------------------------------------------------------------------------
import GraphQL.AST
import GraphQL.Lexer
--------------------------------------------------------------------------------
-- | Pretty prints a 'Document'
printDocument
  :: Document
  -> Doc ann
printDocument (Document defs)
  = vsep (printDefinition <$> defs)

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
  (InputObjectTypeExtension (Name name) dirs inputFieldsDefinition)
    = hsep [ pretty ("extend" :: String)
           , pretty ("input" :: String)
           , pretty name
           , printDirectives dirs
           , printInputFieldsDefinition inputFieldsDefinition
           ]

printInputFieldsDefinition :: InputFieldsDefinition -> Doc ann
printInputFieldsDefinition (InputFieldsDefinition []) = mempty
printInputFieldsDefinition (InputFieldsDefinition xs)
  = pretty '{' <+> hsep (printInputValueDefinition <$> xs) <+> pretty '}'

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
  = hsep [ pretty ("extend" :: String)
         , pretty ("enum" :: String)
         , pretty name
         , printDirectives dirs
         , printEnumValuesDefinition enumValuesDefinition
         ]

printEnumValuesDefinition :: EnumValuesDefinition -> Doc ann
printEnumValuesDefinition (EnumValuesDefinition []) = mempty
printEnumValuesDefinition (EnumValuesDefinition enumValues) =
  pretty '{' <+> hsep (printEnumValueDefinition <$> enumValues) <+> pretty '}'

printEnumValueDefinition :: EnumValueDefinition -> Doc ann
printEnumValueDefinition (EnumValueDefinition maybeDescription enumValue directives)
  = mconcat [ maybe mempty (\x -> printDescription x <> space) maybeDescription
            , printEnumValue enumValue
            , space
            , printDirectives directives
            ]

printEnumValue
  :: EnumValue -> Doc a
printEnumValue (EnumValue (Name n)) = pretty n

printUnionTypeExtension :: UnionTypeExtension -> Doc ann
printUnionTypeExtension (UnionTypeExtension (Name name) directives unionMemberTypes)
  = hsep [ pretty ("extend" :: String)
         , pretty ("union" :: String)
         , pretty name
         , printDirectives directives
         , printUnionMemberTypes unionMemberTypes
         ]

printUnionMemberTypes :: UnionMemberTypes -> Doc a
printUnionMemberTypes (UnionMemberTypes []) = mempty
printUnionMemberTypes (UnionMemberTypes namedTypes) =
  pretty '=' <+> hsep (printNamedType <$> namedTypes)

printNamedType :: NamedType -> Doc a
printNamedType (NamedType (Name n)) = pretty '|' <+> pretty n

printScalarTypeExtension :: ScalarTypeExtension -> Doc ann
printScalarTypeExtension (ScalarTypeExtension (Name name) directives)
  = mconcat [ pretty ("extend" :: String)
            , space
            , pretty ("scalar" :: String)
            , space
            , pretty name
            , space
            , printDirectives directives
            ]

printInterfaceTypeExtension :: InterfaceTypeExtension -> Doc ann
printInterfaceTypeExtension
  (InterfaceTypeExtension (Name name) directives fields)
  = mconcat [ pretty ("extend" :: String)
            , space
            , pretty ("interface" :: String)
            , space
            , pretty name
            , space
            , printDirectives directives
            , printFieldsDefinitions fields
            ]

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
     implementsInterfaces
     directives
     fieldsDefinitions
  )
  = hsep [
    pretty ("extend" :: String)
  , pretty ("type" :: String)
  , pretty name
  , printImplementsInterfaces implementsInterfaces
  , printDirectives directives
  , printFieldsDefinitions fieldsDefinitions
  ]

printSchemaExtension :: SchemaExtension -> Doc a
printSchemaExtension (SchemaExtension dirs operationTypeDefs) =
  pretty ("extend" :: String) <+> pretty ("schema" :: String) <+>
    mconcat [ printDirectives dirs
            , pretty '{' <+>
                vsep (printOperationTypeDefinitions operationTypeDefs)
                  <+> pretty '}'
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
    = mconcat
      [ maybe mempty (\x -> printDescription x <> space) maybeDescription
      , pretty ("directive" :: String)
      , space
      , pretty '@'
      , space
      , pretty name
      , space
      , printArgumentDefinitions argsDefs
      , space
      , pretty ("on" :: String)
      , space
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
printDescription (Description s) = printStringValue s

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
  mconcat [ pretty '&' <+> pretty name <> space
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
    , space
    , printDirectives directives
    , printFieldsDefinitions fields
    ]

printUnionTypeDefinition :: UnionTypeDefinition -> Doc a
printUnionTypeDefinition
  (UnionTypeDefinition maybeDescription
                        (Name name)
                        directives
                        unionMemberTypes)
  = mconcat [
      maybe mempty (\x -> printDescription x <> space) maybeDescription
    , pretty ("union" :: String)
    , space
    , pretty name
    , space
    , printDirectives directives
    , printUnionMemberTypes unionMemberTypes
    ]

printEnumTypeDefinition :: EnumTypeDefinition -> Doc a
printEnumTypeDefinition
  (EnumTypeDefinition maybeDescription
                       (Name name)
                       directives
                       enumValuesDefinition)
  = mconcat [
      maybe mempty (\x -> printDescription x <> space) maybeDescription
    , pretty ("enum" :: String)
    , space
    , pretty name
    , space
    , printDirectives directives
    , printEnumValuesDefinition enumValuesDefinition
    ]

printInputObjectTypeDefinition :: InputObjectTypeDefinition -> Doc a
printInputObjectTypeDefinition
  (InputObjectTypeDefinition maybeDescription
                       (Name name)
                       directives
                       inputFieldsDefinitions)
  = mconcat [
      maybe mempty (\x -> printDescription x <> space) maybeDescription
    , pretty ("input" :: String)
    , space
    , pretty name
    , space
    , printDirectives directives
    , printInputFieldsDefinition inputFieldsDefinitions
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
      = pretty '|' <+> pretty (show l)
    printDirectiveLocation (LocationTypeSystemDirective l)
      = pretty '|' <+> pretty (show l)

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

printStringValue :: StringValue -> Doc ann
printStringValue (StringValue BlockString s) =
  pretty ("\"\"\"" :: String) <> pretty s <> pretty ("\"\"\"" :: String)
printStringValue (StringValue SingleLine s) =
  pretty ("\"" :: String) <> pretty s <> pretty ("\"" :: String)

printValue :: Value -> Doc ann
printValue (ValueString s)      = printStringValue s
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
