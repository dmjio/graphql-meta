{
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.Parser
-- Description : Parser for GraphQL specification
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.Parser
  ( value
  , selSet
  , opDef
  , gDef
  , exeDef
  , defs
  , objType
  , parseDocument
  ) where
--------------------------------------------------------------------------------
import qualified Data.Text as T
import           Data.Text (Text)
import Data.ByteString (ByteString)
--------------------------------------------------------------------------------
import GraphQL.Lexer
import GraphQL.AST
--------------------------------------------------------------------------------
}

%name parseDoc Document
%name parseDefs Definitions
%name parseValue Value
%name parseDef Definition
%name parseObjType ObjectTypeDefinition
%name parseSelSet SelectionSet
%name parseOpDef OperationDefinition
%name parseExeDef ExecutableDefinition

%monad { Either String } { (>>=) } { return }

%tokentype { Token }
%error { parseError }

%token
  int		{ TokenInt $$ }
  float		{ TokenFloat $$ }
  schema        { TokenReserved "schema" }
  scalar        { TokenReserved "scalar" }
  implements    { TokenReserved "implements" }
  interface     { TokenReserved "interface" }
  union         { TokenReserved "union" }
  directive     { TokenReserved "directive" }
  on            { TokenReserved "on" }
  enum          { TokenReserved "enum" }
  input         { TokenReserved "input" }
  type          { TokenReserved "type" }
  extend        { TokenReserved "extend" }
  fragment      { TokenReserved "fragment" }
  name		{ TokenName $$ }
  string	{ TokenString $$ }
  bool		{ TokenBool $$ }
  null		{ TokenNull }
  operationType { TokenOperator $$ }
  executableDirectiveLocation { TokenExecutableDirectiveLocation $$ }
  typeSystemDirectiveLocation { TokenTypeSystemDirectiveLocation $$ }
  '&'           { TokenPunctuator '&' }
  '!'           { TokenPunctuator '!' }
  '$'           { TokenPunctuator '$' }
  '('           { TokenPunctuator '(' }
  ')'           { TokenPunctuator ')' }
  '...'         { TokenMultiPunctuator "..." }
  ':'           { TokenPunctuator ':' }
  '='           { TokenPunctuator '=' }
  '@'           { TokenPunctuator '@' }
  '['           { TokenPunctuator '[' }
  ']'           { TokenPunctuator ']' }
  '{'           { TokenPunctuator '{' }
  '|'           { TokenPunctuator '|' }
  '}'           { TokenPunctuator '}' }

%%

Document :: { Document }
  : { Document [] }
  | Definitions { Document (reverse $1) }

Definitions :: { [Definition] }
  : Definition { [$1] }
  | Definitions Definition { $2 : $1 }

Definition :: { Definition }
Definition
  : ExecutableDefinition { DefinitionExecutable $1 }
  | TypeSystemDefinition { DefinitionTypeSystem $1 }
  | TypeSystemExtension  { ExtensionTypeSystem $1  }

ExecutableDefinition :: { ExecutableDefinition }
  : OperationDefinition { DefinitionOperation $1 }
  | FragmentDefinition { DefinitionFragment $1 }

FragmentSpread :: { FragmentSpread }
  : '...' Name MaybeDirectives { FragmentSpread $2 $3 }

FragmentDefinition :: { FragmentDefinition }
  : fragment Name TypeCondition MaybeDirectives SelectionSet
  { FragmentDefinition $2 $3 $4 $5 }

TypeSystemExtension :: { TypeSystemExtension }
  : SchemaExtension { ExtensionSchema $1 }
  | TypeExtension { ExtensionType $1 }

SchemaExtension :: { SchemaExtension }
  : extend schema MaybeDirectives MaybeOperationTypeDefinition
  { SchemaExtension $3 $4 }

MaybeOperationTypeDefinition :: { [OperationTypeDefinition] }
  : { [] }
  | '{' OperationTypeDefinitions '}' { reverse $2 }

OperationTypeDefinitions :: { [OperationTypeDefinition] }
  : { [] }
  | OperationTypeDefinitions OperationTypeDefinition { $2 : $1 }

OperationTypeDefinition :: { OperationTypeDefinition }
  : operationType ':' NamedType { OperationTypeDefinition $1 $3 }

TypeExtension :: { TypeExtension }
  : ScalarTypeExtension { ExtensionScalarType $1 }
  | ObjectTypeExtension { ExtensionObjectType $1 }
  | InterfaceTypeExtension { ExtensionInterfaceType $1 }
  | UnionTypeExtension { ExtensionUnionType $1 }
  | EnumTypeExtension { ExtensionEnumType $1 }
  | InputObjectTypeExtension { ExtensionInputObjectType $1 }

TypeSystemDefinition :: { TypeSystemDefinition }
  : SchemaDefinition { DefinitionSchema $1 }
  | TypeDefinition { DefinitionType $1 }
  | DirectiveDefinition { DefinitionDirective $1 }

TypeDefinition :: { TypeDefinition }
  : ScalarTypeDefinition { DefinitionScalarType $1 }
  | ObjectTypeDefinition { DefinitionObjectType $1 }
  | InterfaceTypeDefinition { DefinitionInterfaceType $1 }
  | UnionTypeDefinition { DefinitionUnionType $1 }
  | EnumTypeDefinition { DefinitionEnumType $1 }
  | InputObjectTypeDefinition { DefinitionInputObjectType $1 }

ScalarTypeDefinition :: { ScalarTypeDefinition }
  : MaybeDescription scalar Name MaybeDirectives
  { ScalarTypeDefinition $1 $3 $4 }

InputObjectTypeDefinition :: { InputObjectTypeDefinition }
  : MaybeDescription input Name MaybeDirectives MaybeInputFieldsDefinition
  { InputObjectTypeDefinition $1 $3 $4 $5 }

UnionTypeDefinition :: { UnionTypeDefinition }
  : MaybeDescription union Name MaybeDirectives MaybeUnionMemberTypes
  { UnionTypeDefinition $1 $3 $4 $5 }

EnumTypeDefinition :: { EnumTypeDefinition }
  : MaybeDescription enum Name MaybeDirectives MaybeEnumValuesDefinition
  { EnumTypeDefinition $1 $3 $4 $5 }

UnionMemberTypes :: { [NamedType] }
  : '=' NamedType { [$2] }
  | '=' '|' NamedType { [$3] }
  | UnionMemberTypes '|' NamedType { $3 : $1 }

InterfaceTypeDefinition :: { InterfaceTypeDefinition }
  : MaybeDescription interface Name MaybeDirectives MaybeFieldsDefinition
  { InterfaceTypeDefinition $1 $3 $4 $5 }

ObjectTypeDefinition :: { ObjectTypeDefinition }
  : MaybeDescription type Name MaybeImplementsInterfaces MaybeDirectives MaybeFieldsDefinition
  { ObjectTypeDefinition $1 $3 $4 $5 $6 }

SchemaDefinition :: { SchemaDefinition }
  : schema MaybeDirectives '{' OperationTypeDefinitions '}'
  { SchemaDefinition $2 (reverse $4) }

FieldsDefinition :: { [FieldDefinition] }
  : '{' Fields '}' { $2 }

ScalarTypeExtension :: { ScalarTypeExtension }
  : extend scalar Name MaybeDirectives { ScalarTypeExtension $3 $4 }

ObjectTypeExtension :: { ObjectTypeExtension }
  : extend type Name MaybeImplementsInterfaces MaybeDirectives MaybeFieldsDefinition
  { ObjectTypeExtension $3 $4 $5 $6 }

InterfaceTypeExtension :: { InterfaceTypeExtension }
  : extend interface Name MaybeDirectives MaybeFieldsDefinition
    { InterfaceTypeExtension $3 $4 $5 }

UnionTypeExtension :: { UnionTypeExtension }
  : extend union Name MaybeDirectives MaybeUnionMemberTypes
    { UnionTypeExtension $3 $4 $5 }

EnumTypeExtension :: { EnumTypeExtension }
  : extend enum Name MaybeDirectives MaybeEnumValuesDefinition
    { EnumTypeExtension $3 $4 $5 }

MaybeEnumValuesDefinition :: { EnumValuesDefinition }
  : { EnumValuesDefinition [] }
  | '{' EnumValuesDefinition '}' { EnumValuesDefinition (reverse $2) }

EnumValuesDefinition :: { [EnumValueDefinition] }
  : EnumValueDefinition { [$1] }
  | EnumValuesDefinition EnumValueDefinition { $2 : $1 }

EnumValueDefinition :: { EnumValueDefinition }
 : MaybeDescription EnumValue MaybeDirectives { EnumValueDefinition $1 $2 $3 }

InputObjectTypeExtension :: { InputObjectTypeExtension }
  : extend input Name MaybeDirectives MaybeInputFieldsDefinition
  { InputObjectTypeExtension $3 $4 $5 }

Fields :: { [FieldDefinition] }
  : FieldDefinition { [$1] }
  | Fields FieldDefinition { $2 : $1 }

FieldDefinition :: { FieldDefinition }
  : MaybeDescription Name MaybeArgumentsDefinition ':' Type MaybeDirectives
  { FieldDefinition $1 $2 $3 $5 $6 }

OperationDefinition :: { OperationDefinition }
  : operationType MaybeName VariableDefinitions MaybeDirectives SelectionSet
    { OperationDefinition $1 $2 $3 $4 $5 }
  | SelectionSet { AnonymousQuery $1 }

DirectiveDefinition :: { DirectiveDefinition }
  : MaybeDescription directive '@' Name MaybeArgumentsDefinition on DirectiveLocations
   { DirectiveDefinition $1 $4 $5 $7 }

DirectiveLocations :: { DirectiveLocations }
  : '|' DirectiveLocation { [$2] }
  | DirectiveLocation { [$1] }
  | DirectiveLocations '|' DirectiveLocation { $3 : $1 }

DirectiveLocation :: { DirectiveLocation }
  : executableDirectiveLocation { LocationExecutableDirective $1 }
  | typeSystemDirectiveLocation { LocationTypeSystemDirective $1 }

MaybeArgumentsDefinition :: { ArgumentsDefinition }
  : { ArgumentsDefinition [] }
  | ArgumentsDefinition { ArgumentsDefinition $1 }

ArgumentsDefinition :: { [InputValueDefinition] }
  : '(' InputValuesDefinition ')' { (reverse $2) }

InputValuesDefinition :: { [InputValueDefinition] }
  : InputValueDefinition { [$1] }
  | InputValuesDefinition InputValueDefinition { $2 : $1 }

InputValueDefinition :: { InputValueDefinition }
  : MaybeDescription Name ':' Type MaybeDefaultValue MaybeDirectives
  { InputValueDefinition $1 $2 $4 $5 $6 }

MaybeFieldsDefinition :: { FieldsDefinition }
  : { FieldsDefinition [] }
  | FieldsDefinition { FieldsDefinition (reverse $1) }

MaybeDirectives :: { Directives }
  : { [] }
  | Directives { reverse $1 }

MaybeInputFieldsDefinition :: { InputFieldsDefinition }
  : { InputFieldsDefinition [] }
  | '{' InputValueDefinitions '}' { InputFieldsDefinition (reverse $2) }

InputValueDefinitions :: { [InputValueDefinition] }
  : InputValueDefinition { [$1] }
  | InputValueDefinitions InputValueDefinition { $2 : $1 }

MaybeUnionMemberTypes :: { UnionMemberTypes }
  : { UnionMemberTypes [] }
  | UnionMemberTypes { UnionMemberTypes (reverse $1) }

MaybeImplementsInterfaces :: { ImplementsInterfaces }
  : { ImplementsInterfaces [] }
  | ImplementsInterfaces { ImplementsInterfaces (reverse $1) }

ImplementsInterfaces :: { [NamedType] }
  : implements '&' NamedType { [$3] }
  | implements NamedType { [$2] }
  | ImplementsInterfaces '&' NamedType { $3 : $1  }

MaybeDescription :: { Maybe Description }
  : { Nothing }
  | Description { Just $1 }

Description :: { Description }
  : string { Description $1 }

MaybeName :: { Maybe Name }
  : { Nothing }
  | Name { Just $1 }

MaybeDefaultValue :: { Maybe DefaultValue }
  : { Nothing }
  | DefaultValue { Just $1 }

Alias :: { Alias }
  : Name ':' { Alias $1 }

VariableDefinitions :: { VariableDefinitions }
  : { [] }
  | '(' VariableDefinitionList ')' { reverse $2 }

VariableDefinitionList :: { VariableDefinitions }
  : { [] }
  | VariableDefinitionList VariableDefinition { $2 : $1 }

Directives :: { [Directive] }
  : Directive { [$1] }
  | Directives Directive { $2 : $1 }

SelectionSet :: { SelectionSet }
  : '{' SelectionSets '}' { reverse $2 }

SelectionSets :: { SelectionSet }
  : { [] }
  | SelectionSets Selection { $2 : $1 }

Arguments :: { Arguments }
  : { [] }
  | '(' ArgumentList ')' { reverse $2 }

ArgumentList :: { Arguments }
  : { [] }
  | ArgumentList Argument { $2 : $1 }

Directive :: { Directive }
  : '@' Name Arguments { Directive $2 $3 }

Selection :: { Selection }
  : Field { SelectionField $1 }
  | FragmentSpread { SelectionFragmentSpread $1 }
  | InlineFragment { SelectionInlineFragment $1 }

InlineFragment :: { InlineFragment }
  : '...' MaybeTypeCondition MaybeDirectives SelectionSet { InlineFragment $2 $3 $4 }

MaybeTypeCondition :: { Maybe TypeCondition }
  : { Nothing}
  | TypeCondition { Just $1 }

Field :: { Field }
  : Alias Name2 Arguments MaybeDirectives MaybeSelectionSet { Field (Just $1) $2 $3 $4 $5 }
  | Name2 Arguments MaybeDirectives MaybeSelectionSet { Field Nothing $1 $2 $3 $4 }

MaybeSelectionSet :: { SelectionSet }
  : { [] }
  | SelectionSet { $1 }

VariableDefinition :: { VariableDefinition }
  : Variable ':' Type DefaultValue { VariableDefinition $1 $3 (Just $4) }
  | Variable ':' Type { VariableDefinition $1 $3 Nothing }

Argument :: { Argument }
  : Name ':' Value { Argument $1 $3 }

TypeCondition :: { TypeCondition }
  : on NamedType { TypeCondition $2 }

Value :: { Value }
  : Variable { ValueVariable $1 }
  | int { ValueInt $1 }
  | float { ValueFloat $1 }
  | string { ValueString $1 }
  | bool { ValueBoolean $1 }
  | null { ValueNull }
  | Name { ValueEnum (EnumValue $1) }
  | '[' ValueList ']' { ValueList (reverse $2) }
  | '{' ValueObjects '}' { ValueObject (reverse $2) }

DefaultValue :: { DefaultValue }
  : '=' Value { DefaultValue $2 }

Variable :: { Variable }
  : '$' Name { Variable $2 }

ValueList :: { [Value] }
  : { [] }
  | ValueList Value { $2 : $1 }

ValueObjects :: { [ObjectField] }
  : { [] }
  | ValueObjects ObjectField { $2 : $1 }

ObjectField :: { ObjectField }
  : Name ':' Value { ObjectField $1 $3 }

Name2 :: { Name }
  : type { Name "type" }
  | name { Name $1 }

Name :: { Name }
  : name { Name $1 }

EnumValue :: { EnumValue }
  : Name { EnumValue $1 }

Type :: { Type }
  : NamedType { TypeNamed $1 }
  | ListType { TypeList $1 }
  | NonNullType { TypeNonNull $1 }

NamedType :: { NamedType }
  : Name { NamedType $1 }

ListType :: { ListType }
  : '[' Type ']' { ListType $2 }

NonNullType :: { NonNullType }
  : NamedType '!' { NonNullTypeNamed $1 }
  | ListType '!' { NonNullTypeList $1 }

{

-- | Parses a GraphQL 'Value'
value :: ByteString -> Either String Value
value = parseValue . getTokens

-- | Parses a GraphQL 'SelectionSet'
selSet :: ByteString -> Either String SelectionSet
selSet = parseSelSet . getTokens

-- | Parses a GraphQL 'OperationDefinition'
opDef :: ByteString -> Either String OperationDefinition
opDef = parseOpDef . getTokens

-- | Parses a GraphQL 'OperationDefinition'
objType :: ByteString -> Either String ObjectTypeDefinition
objType = parseObjType . getTokens

-- | Parses a GraphQL '[Definition]'
defs :: ByteString -> Either String [Definition]
defs = parseDefs . getTokens

-- | Parses a GraphQL 'Definition'
gDef :: ByteString -> Either String Definition
gDef = parseDef . getTokens

-- | Parses a GraphQL 'ExecutableDefinition'
exeDef :: ByteString -> Either String ExecutableDefinition
exeDef = parseExeDef . getTokens

-- | Parses a GraphQL 'Document'
parseDocument :: ByteString -> Either String Document
parseDocument = parseDoc . getTokens

parseError :: [Token] -> Either String a
parseError tks = Left (show tks)
    {-
  Left $ "Parse error: " <> T.unpack (explainToken (head tks))
    where
      explainToken (TokenError err) = explainError err
      explainToken t = T.pack (show t)
      explainError (ConversionError errMsg s)
        = errMsg <> " at " <> s
      explainError (LexerError errMsg)
        = errMsg
     -}
}
