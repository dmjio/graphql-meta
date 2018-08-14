{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Test.GraphQL.Gen
-- Description : Test fixture generators for GraphQL AST
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Test.GraphQL.Gen where
--------------------------------------------------------------------------------
import           Control.Monad
import           Data.Char
import           Test.QuickCheck
import qualified Data.Text       as T
import           Data.Text       (Text)
--------------------------------------------------------------------------------
import           GraphQL.AST
import           GraphQL.Lexer
--------------------------------------------------------------------------------
-- | Generates a valid GraphQL 'Name'
-- http://facebook.github.io/graphql/draft/#Name
genName :: Gen Name
genName = do
  r <- fmap Name $ T.cons <$> prefix <*> suffix
  if r == Name "on"
    then genName
    else pure r
      where
        prefix :: Gen Char
        prefix =
          oneof [
            choose ('a', 'z')
          , choose ('A', 'Z')
          , pure '_'
          ]
        suffix :: Gen Text
        suffix = T.pack <$> do
          k <- choose (1, 5)
          replicateM k $
            oneof [
              choose ('a', 'z')
            , choose ('A', 'Z')
            , intToDigit <$> choose (0, 9)
            , pure '_'
            ]

-- | Generate a 'Variable'
genVariable :: Gen Variable
genVariable = Variable <$> genName

-- | Generates a valid GraphQL 'Value'
-- http://facebook.github.io/graphql/draft/#Name
genValue :: Gen Value
genValue =
  oneof [
    ValueVariable <$> genVariable
  , ValueInt <$> arbitrary
  , ValueFloat <$> arbitrary
  , ValueString <$>
      oneof [ genStringCharacters
            , genUnicode
            --, genBlockStringCharacter
            ]
  , ValueBoolean <$> arbitrary
  , pure ValueNull
  , ValueEnum . EnumValue <$> genName
  , ValueList <$> do
      choose (0,2) >>=
        flip replicateM genValue
  , ValueObject <$> do
      choose (0,2) >>=
        flip replicateM genObjectField
  ]

genStringCharacters :: Gen Text
genStringCharacters = T.pack <$> do
  n <- choose (0, 40)
  replicateM n $ elements $ filter (`notElem` ['\n','\r','"','\\']) chars
    where
      chars = [ '\x9', '\xa', '\xd'] ++ ['\x20' .. '\xff']

genBlockStringCharacter :: Gen Text
genBlockStringCharacter = do
  str <- genStringCharacters
  pure ("\"\"\"" <> str <> "\"\"\"")

genUnicode :: Gen Text
genUnicode = T.pack <$> do
  elements $ do
      b <- alpha
      c <- alpha
      d <- alpha
      e <- alpha
      pure $ "\\u" ++ [b,c,d,e]
      where
        alpha = ['a'..'f']
             ++ ['A'..'F']
             ++ ['0'..'9']

genEscapedCharacter :: Gen T.Text
genEscapedCharacter = T.pack <$> do
  elements $ do
    b <- chars
    pure $ "\\" ++ [b]
      where
        chars :: String
        chars = ['\"','\\','/','b','f','n','r','t']


genObjectField :: Gen ObjectField
genObjectField =
  ObjectField
    <$> genName
    <*> genValue

genTypeSystemDefinition :: Gen TypeSystemDefinition
genTypeSystemDefinition =
  oneof [ DefinitionSchema <$> genSchemaDefinition
        , DefinitionType <$> genTypeDefinition
--      , DefinitionDirective <$> genDirectiveDefinition
        ]

genDocument :: Gen Document
genDocument = Document <$> do
  choose (0,2) >>= flip replicateM genDefinition

genSchemaDefinition :: Gen SchemaDefinition
genSchemaDefinition =
  SchemaDefinition
    <$> genDirectives
    <*> genOperationTypeDefinitions

genOperationTypeDefinitions :: Gen [OperationTypeDefinition]
genOperationTypeDefinitions =
  choose (0,2) >>= flip replicateM genOperationTypeDefinition

genOperationTypeDefinition :: Gen OperationTypeDefinition
genOperationTypeDefinition =
  OperationTypeDefinition
    <$> genOperationType
    <*> genNamedType

genTypeDefinition :: Gen TypeDefinition
genTypeDefinition = oneof
  [ DefinitionScalarType <$> genScalarTypeDefinition
  , DefinitionObjectType <$> genObjectTypeDefinition
  , DefinitionInterfaceType <$> genInterfaceTypeDefinition
  , DefinitionUnionType <$> genUnionTypeDefinition
  , DefinitionEnumType <$> genEnumTypeDefinition
  , DefinitionInputObjectType <$> genInputObjectTypeDefinition
  ]

genInterfaceTypeDefinition :: Gen InterfaceTypeDefinition
genInterfaceTypeDefinition =
  InterfaceTypeDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genDirectives
    <*> genFieldsDefinition

genUnionTypeDefinition :: Gen UnionTypeDefinition
genUnionTypeDefinition =
  UnionTypeDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genDirectives
    <*> genUnionMemberTypes

genUnionMemberTypes :: Gen UnionMemberTypes
genUnionMemberTypes = UnionMemberTypes <$> do
  choose (0,2) >>= flip replicateM genNamedType

genEnumTypeDefinition :: Gen EnumTypeDefinition
genEnumTypeDefinition =
  EnumTypeDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genDirectives
    <*> genEnumValuesDefinition

genEnumValuesDefinition :: Gen EnumValuesDefinition
genEnumValuesDefinition = EnumValuesDefinition <$> do
  choose (1,2) >>= flip replicateM genEnumValueDefinition

genEnumValueDefinition :: Gen EnumValueDefinition
genEnumValueDefinition =
  EnumValueDefinition
    <$> genMaybe genDescription
    <*> genEnumValue
    <*> genDirectives

genEnumValue :: Gen EnumValue
genEnumValue = EnumValue <$> genName

genInputObjectTypeDefinition :: Gen InputObjectTypeDefinition
genInputObjectTypeDefinition =
  InputObjectTypeDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genDirectives
    <*> genInputFieldsDefinition

genInputFieldsDefinition :: Gen InputFieldsDefinition
genInputFieldsDefinition =
  InputFieldsDefinition <$> do
    choose (1,2) >>= flip replicateM genInputValueDefinition

genInputFieldDefinition :: Gen InputValueDefinition
genInputFieldDefinition =
  InputValueDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genType
    <*> genMaybe genDefaultValue
    <*> genDirectives

genObjectTypeDefinition :: Gen ObjectTypeDefinition
genObjectTypeDefinition =
  ObjectTypeDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genImplementsInterfaces
    <*> genDirectives
    <*> genFieldsDefinition

genImplementsInterfaces :: Gen ImplementsInterfaces
genImplementsInterfaces =
  ImplementsInterfaces <$> do
    choose (0,2) >>= flip replicateM genNamedType

-- | Can't be empty, or ambiguities occur w/ anonymous queries
genFieldsDefinition :: Gen FieldsDefinition
genFieldsDefinition = FieldsDefinition <$> do
  choose (1,2) >>= flip replicateM genFieldDefinition

genFieldDefinition :: Gen FieldDefinition
genFieldDefinition =
  FieldDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genArgumentsDefinition
    <*> genType
    <*> genDirectives

genScalarTypeDefinition :: Gen ScalarTypeDefinition
genScalarTypeDefinition =
  ScalarTypeDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genDirectives

genDescription :: Gen Description
genDescription =
  Description <$>
    genStringCharacters

genMaybe :: Gen a -> Gen (Maybe a)
genMaybe gen =
  oneof [ pure Nothing
        , Just <$> gen
        ]

genDirectiveDefinition :: Gen DirectiveDefinition
genDirectiveDefinition =
  DirectiveDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genArgumentsDefinition
    <*> genDirectiveLocations

genArgumentsDefinition :: Gen ArgumentsDefinition
genArgumentsDefinition = ArgumentsDefinition <$> do
  choose (1,2) >>= flip replicateM genInputValueDefinition

genInputValueDefinition :: Gen InputValueDefinition
genInputValueDefinition =
  InputValueDefinition
    <$> genMaybe genDescription
    <*> genName
    <*> genType
    <*> genMaybe genDefaultValue
    <*> genDirectives

genDirectiveLocations :: Gen DirectiveLocations
genDirectiveLocations =
  choose (1,2) >>= flip replicateM genDirectiveLocation

genDirectiveLocation :: Gen DirectiveLocation
genDirectiveLocation =
  oneof [ LocationExecutableDirective <$> genExecutableDirectiveLocation
        , LocationTypeSystemDirective <$> genTypeSystemDirectiveLocation
        ]

genExecutableDirectiveLocation :: Gen ExecutableDirectiveLocation
genExecutableDirectiveLocation =
  oneof $ pure <$> [ QUERY .. ]

genTypeSystemDirectiveLocation :: Gen TypeSystemDirectiveLocation
genTypeSystemDirectiveLocation =
  oneof $ pure <$> [ SCHEMA .. ]

genExecutableDefinition :: Gen ExecutableDefinition
genExecutableDefinition =
  oneof [ DefinitionOperation <$> genOperationDefinition
        , DefinitionFragment <$> genFragmentDefinition
        ]

genOperationDefinition :: Gen OperationDefinition
genOperationDefinition =
  oneof [ AnonymousQuery <$> genSelectionSet
        , OperationDefinition
             <$> genOperationType
             <*> oneof [ pure Nothing
                       , Just <$> genName
                       ]
             <*> genVariableDefinitions
             <*> genDirectives
             <*> genSelectionSet
        ]

genOperationType :: Gen OperationType
genOperationType =
  oneof [ pure Query
        , pure Subscription
        , pure Mutation
        ]

genDefinition :: Gen Definition
genDefinition =
  oneof [ DefinitionExecutable <$> genExecutableDefinition
        , DefinitionTypeSystem <$> genTypeSystemDefinition
--      , ExtensionTypeSystem <$> genTypeSystemExtension
        ]

genTypeSystemExtension :: Gen TypeSystemExtension
genTypeSystemExtension =
  oneof [ ExtensionSchema <$> genSchemaExtension
        , ExtensionType <$> genTypeExtension
        ]

genSchemaExtension :: Gen SchemaExtension
genSchemaExtension = undefined

genTypeExtension :: Gen TypeExtension
genTypeExtension = oneof
  [ ExtensionScalarType <$> genScalarTypeExtension
  , ExtensionObjectType <$> genObjectTypeExtension
  , ExtensionInterfaceType <$> genInterfaceTypeExtension
  , ExtensionUnionType <$> genUnionTypeExtension
  , ExtensionEnumType <$> genEnumTypeExtension
  , ExtensionInputObjectType <$> genInputObjectTypeExtension
  ]

genScalarTypeExtension :: Gen ScalarTypeExtension
genScalarTypeExtension =
  ScalarTypeExtension
    <$> genName
    <*> genDirectives

genObjectTypeExtension :: Gen ObjectTypeExtension
genObjectTypeExtension =
  ObjectTypeExtension
    <$> genName
    <*> genImplementsInterfaces
    <*> genDirectives
    <*> genFieldsDefinition

genInterfaceTypeExtension :: Gen InterfaceTypeExtension
genInterfaceTypeExtension =
  InterfaceTypeExtension
    <$> genName
    <*> genDirectives
    <*> genFieldsDefinition

genUnionTypeExtension :: Gen UnionTypeExtension
genUnionTypeExtension =
  UnionTypeExtension
    <$> genName
    <*> genDirectives
    <*> genUnionMemberTypes

genEnumTypeExtension :: Gen EnumTypeExtension
genEnumTypeExtension =
  EnumTypeExtension
    <$> genName
    <*> genDirectives
    <*> genEnumValuesDefinition

genInputObjectTypeExtension :: Gen InputObjectTypeExtension
genInputObjectTypeExtension =
  InputObjectTypeExtension
    <$> genName
    <*> genDirectives
    <*> genInputFieldsDefinition

genVariableDefinitions :: Gen VariableDefinitions
genVariableDefinitions =
  choose (0,2) >>= flip replicateM genVariableDefinition

genVariableDefinition :: Gen VariableDefinition
genVariableDefinition =
  VariableDefinition
    <$> genVariable
    <*> genType
    <*> oneof [ pure Nothing
              , Just <$> genDefaultValue
              ]

genType :: Gen Type
genType =
  oneof [ TypeNamed <$> genNamedType
        , TypeList <$> genListType
        , TypeNonNull <$> genNonNullType
        ]

genNamedType :: Gen NamedType
genNamedType = NamedType <$> genName

genNonNullType :: Gen NonNullType
genNonNullType =
  oneof [ NonNullTypeNamed <$> genNamedType
        , NonNullTypeList <$> genListType
        ]

genListType :: Gen ListType
genListType = ListType <$> genType

genDefaultValue :: Gen DefaultValue
genDefaultValue = DefaultValue <$> genValue

genDirective :: Gen Directive
genDirective = Directive <$> genName <*> genArguments

genArguments :: Gen Arguments
genArguments = choose (0,2) >>= flip replicateM genArgument

genArgument :: Gen Argument
genArgument = Argument <$> genName <*> genValue

genDirectives :: Gen Directives
genDirectives = choose (0,2) >>= flip replicateM genDirective

genSelection :: Gen Selection
genSelection =
  oneof [ SelectionField <$> genField
        , SelectionFragmentSpread <$> genFragmentSpread
        , SelectionInlineFragment <$> genSelectionInlineFragment
        ]

genField :: Gen Field
genField =
  Field <$> oneof [ pure Nothing, Just <$> genAlias ]
        <*> genName
        <*> genArguments
        <*> genDirectives
        <*> genSelectionSet

genAlias :: Gen Alias
genAlias = Alias <$> genName

genFragmentSpread :: Gen FragmentSpread
genFragmentSpread = FragmentSpread <$> genName <*> genDirectives

genSelectionInlineFragment :: Gen InlineFragment
genSelectionInlineFragment =
  InlineFragment
    <$> oneof [ pure Nothing, Just <$> genTypeCondition ]
    <*> genDirectives
    <*> genSelectionSet

genTypeCondition :: Gen TypeCondition
genTypeCondition = TypeCondition <$> genNamedType

genSelectionSet :: Gen SelectionSet
genSelectionSet = choose (0,2) >>= flip replicateM genSelection

genFragmentDefinition :: Gen FragmentDefinition
genFragmentDefinition =
  FragmentDefinition
    <$> genName
    <*> genTypeCondition
    <*> genDirectives
    <*> genSelectionSet
