{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.AST
-- Description : GraphQL abstract syntax tree representation
-- Maintainer  : David Johnson <david@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.AST where
--------------------------------------------------------------------------------
import GHC.Generics    (Generic)
import Data.Typeable   (Typeable)
import Data.Data       (Data)
import Data.Text       (Text)
import Control.DeepSeq (NFData)
#if MIN_VERSION_base(4,10,0)
import Data.Semigroup  (Semigroup)
#else
#endif
--------------------------------------------------------------------------------
import GraphQL.Lexer
--------------------------------------------------------------------------------

-- | A GraphQL 'Document'
-- http://facebook.github.io/graphql/draft/#sec-Language.Document
newtype Document
  = Document [ Definition ]
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | A GraphQL 'Definition'
-- http://facebook.github.io/graphql/draft/#Definition
data Definition
  = DefinitionExecutable ExecutableDefinition
  | DefinitionTypeSystem TypeSystemDefinition
  | ExtensionTypeSystem TypeSystemExtension
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Definition

-- | A GraphQL 'ExecutableDefinition'
-- http://facebook.github.io/graphql/draft/#ExecutableDefinitxion
data ExecutableDefinition
  = DefinitionOperation OperationDefinition
  | DefinitionFragment FragmentDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData ExecutableDefinition

-- | A GraphQL 'OperationDefinition'
-- http://facebook.github.io/graphql/draft/#ExecutableDefinition
data OperationDefinition
  = AnonymousQuery SelectionSet
  | OperationDefinition
      OperationType
      (Maybe Name)
      [VariableDefinition]
      [Directive]
      SelectionSet
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData OperationDefinition

-- | A GraphQL 'SelectionSet'
-- http://facebook.github.io/graphql/draft/#sec-Selection-Sets
type SelectionSet = [Selection]

-- | A GraphQL 'Selection' type
-- http://facebook.github.io/graphql/draft/#sec-Selection
data Selection
  = SelectionField Field
  | SelectionFragmentSpread FragmentSpread
  | SelectionInlineFragment InlineFragment
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Selection

-- | A GraphQL 'Field' type
-- http://facebook.github.io/graphql/draft/#sec-Field
data Field
  = Field (Maybe Alias) Name Arguments Directives SelectionSet
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Field

-- | A GraphQL 'Alias'
-- http://facebook.github.io/graphql/draft/#Alias
newtype Alias = Alias Name
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | GraphQL 'Arguments'
-- http://facebook.github.io/graphql/draft/#Arguments
type Arguments = [Argument]

-- | A GraphQL 'Argument'
-- http://facebook.github.io/graphql/draft/#Argument
data Argument = Argument Name Value
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Argument

-- | GraphQL 'FragmentSpread' type
-- http://facebook.github.io/graphql/draft/#sec-FragmentSpread
data FragmentSpread
  = FragmentSpread
    Name
    Directives
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData FragmentSpread

-- | GraphQL 'InlineFragment' type
-- http://facebook.github.io/graphql/draft/#sec-InlineFragment
data InlineFragment
  = InlineFragment
    (Maybe TypeCondition)
    Directives
    SelectionSet
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData InlineFragment

-- | A GraphQL 'FragmentDefinition'
-- http://facebook.github.io/graphql/draft/#FragmentDefinition
data FragmentDefinition
  = FragmentDefinition
    Name
    TypeCondition
    Directives
    SelectionSet
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData FragmentDefinition

-- | A GraphQL 'FragmentName'
-- http://facebook.github.io/graphql/draft/#FragmentName
newtype FragmentName = FragmentName Name
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)

-- | A GraphQL 'TypeCondition'
-- http://facebook.github.io/graphql/draft/#TypeCondition
newtype TypeCondition = TypeCondition NamedType
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)

-- | A GraphQL 'Value'
-- http://facebook.github.io/graphql/draft/#Value
data Value
  = ValueVariable Variable
  | ValueInt Int
  | ValueFloat Double
  | ValueString Text
  | ValueBoolean Bool
  | ValueNull
  | ValueEnum EnumValue
  | ValueList [Value]
  | ValueObject [ObjectField]
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Value

-- | A GraphQL 'EnumValue'
-- http://facebook.github.io/graphql/draft/#EnumValue
newtype EnumValue = EnumValue Name
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)

-- | A GraphQL 'ObjectField'
-- http://facebook.github.io/graphql/draft/#ObjectField
data ObjectField = ObjectField Name Value
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData ObjectField

-- | GraphQL 'VariableDefinitions'
-- http://facebook.github.io/graphql/draft/#VariableDefinitions
type VariableDefinitions = [VariableDefinition]

-- | A GraphQL 'VariableDefinition'
-- http://facebook.github.io/graphql/draft/#VariableDefinition
data VariableDefinition
  = VariableDefinition Variable Type (Maybe DefaultValue)
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData VariableDefinition

-- | A GraphQL 'Variable'
-- http://facebook.github.io/graphql/draft/#Variable
newtype Variable = Variable Name
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)

-- | A GraphQL 'DefaultValue'
-- http://facebook.github.io/graphql/draft/#DefaultValue
newtype DefaultValue = DefaultValue Value
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | A GraphQL 'Type'
-- http://facebook.github.io/graphql/draft/#Type
data Type
  = TypeNamed NamedType
  | TypeList ListType
  | TypeNonNull NonNullType
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Type

-- | A GraphQL 'NamedType'
-- http://facebook.github.io/graphql/draft/#NamedType
newtype NamedType = NamedType Name
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)

-- | A GraphQL 'ListType'
-- http://facebook.github.io/graphql/draft/#ListType
newtype ListType = ListType Type
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | A GraphQL 'NonNullType'
-- http://facebook.github.io/graphql/draft/#NonNullType
data NonNullType
  = NonNullTypeNamed NamedType
  | NonNullTypeList ListType
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData NonNullType

-- | The GraphQL 'Directives' type
-- http://facebook.github.io/graphql/draft/#sec-Directives
type Directives = [Directive]

-- | A GraphQL 'Directive'
-- http://facebook.github.io/graphql/draft/#Directive
data Directive
  = Directive Name Arguments
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData Directive

-- | A GraphQL 'TypeSystemDefinition'
-- http://facebook.github.io/graphql/draft/#TypeSystemDefinition
data TypeSystemDefinition
  = DefinitionSchema SchemaDefinition
  | DefinitionType TypeDefinition
  | DefinitionDirective DirectiveDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData TypeSystemDefinition

-- | A GraphQL 'TypeSystemExtension'
-- http://facebook.github.io/graphql/draft/#TypeSystemExtension
data TypeSystemExtension
  = ExtensionSchema SchemaExtension
  | ExtensionType TypeExtension
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData TypeSystemExtension

-- | A GraphQL 'SchemaDefinition'
-- http://facebook.github.io/graphql/draft/#SchemaDefinition
data SchemaDefinition
  = SchemaDefinition
    Directives
    OperationTypeDefinitions
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData SchemaDefinition

-- | A GraphQL 'SchemaExtension'
-- http://facebook.github.io/graphql/draft/#SchemaExtension
data SchemaExtension
  = SchemaExtension Directives OperationTypeDefinitions
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData SchemaExtension

-- | List of 'RootOperationTypeDefinition'
type RootOperationTypeDefinitions = [RootOperationTypeDefinition]

-- | List of 'OperationTypeDefinition'
type OperationTypeDefinitions = [OperationTypeDefinition]

-- | http://facebook.github.io/graphql/draft/#RootOperationTypeDefinition
data RootOperationTypeDefinition
  = RootOperationTypeDefinition OperationType NamedType
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData RootOperationTypeDefinition

-- | http://facebook.github.io/graphql/draft/#OperationTypeDefinition
data OperationTypeDefinition
  = OperationTypeDefinition OperationType NamedType
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData OperationTypeDefinition

-- | A GraphQL 'Description'
-- http://facebook.github.io/graphql/draft/#Description
newtype Description = Description Text
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)

-- | A GraphQL 'TypeDefinition'
-- http://facebook.github.io/graphql/draft/#TypeDefinition
data TypeDefinition
  = DefinitionScalarType ScalarTypeDefinition
  | DefinitionObjectType ObjectTypeDefinition
  | DefinitionInterfaceType InterfaceTypeDefinition
  | DefinitionUnionType UnionTypeDefinition
  | DefinitionEnumType EnumTypeDefinition
  | DefinitionInputObjectType InputObjectTypeDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData TypeDefinition

-- | A GraphQL 'TypeExtension'
-- http://facebook.github.io/graphql/draft/#TypeExtension
data TypeExtension
  = ExtensionScalarType ScalarTypeExtension
  | ExtensionObjectType ObjectTypeExtension
  | ExtensionInterfaceType InterfaceTypeExtension
  | ExtensionUnionType UnionTypeExtension
  | ExtensionEnumType EnumTypeExtension
  | ExtensionInputObjectType InputObjectTypeExtension
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData TypeExtension

-- | A GraphQL 'ScalarTypeDefinition'
-- http://facebook.github.io/graphql/draft/#ScalarTypeDefinition
data ScalarTypeDefinition
  = ScalarTypeDefinition
    (Maybe Description)
    Name
    Directives
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData ScalarTypeDefinition

-- | A GraphQL 'ScalarTypeExtension'
-- http://facebook.github.io/graphql/draft/#ScalarTypeExtension
data ScalarTypeExtension
  = ScalarTypeExtension Name Directives
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData ScalarTypeExtension

-- | A GraphQL 'ObjectTypeDefinition'
-- http://facebook.github.io/graphql/draft/#ObjectTypeDefinition
data ObjectTypeDefinition
  = ObjectTypeDefinition
    (Maybe Description)
    Name
    ImplementsInterfaces
    Directives
    FieldsDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData ObjectTypeDefinition

-- | A GraphQL 'ObjectTypeExtension'
-- http://facebook.github.io/graphql/draft/#ObjectTypeExtension
data ObjectTypeExtension
  = ObjectTypeExtension
    Name
    ImplementsInterfaces
    Directives
    FieldsDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData ObjectTypeExtension

-- | A GraphQL 'ImplementsInterfaces'
-- http://facebook.github.io/graphql/draft/#ImplementsInterfaces
newtype ImplementsInterfaces
  = ImplementsInterfaces [NamedType]
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | A GraphQL 'FieldsDefinition'
-- http://facebook.github.io/graphql/draft/#FieldsDefinition
newtype FieldsDefinition = FieldsDefinition [FieldDefinition]
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | A GraphQL 'FieldDefinition'
-- http://facebook.github.io/graphql/draft/#FieldDefinition
data FieldDefinition
  = FieldDefinition
    (Maybe Description)
    Name
    ArgumentsDefinition
    Type
    Directives
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData FieldDefinition

-- | http://facebook.github.io/graphql/draft/#ArgumentsDefinition
newtype ArgumentsDefinition
  = ArgumentsDefinition [InputValueDefinition]
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | http://facebook.github.io/graphql/draft/#InputValueDefinition
data InputValueDefinition
  = InputValueDefinition
    (Maybe Description)
    Name
    Type
    (Maybe DefaultValue)
    Directives
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData InputValueDefinition

-- | http://facebook.github.io/graphql/draft/#InterfaceTypeDefinition
data InterfaceTypeDefinition
  = InterfaceTypeDefinition
    (Maybe Description)
    Name
    Directives
    FieldsDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData InterfaceTypeDefinition

-- | http://facebook.github.io/graphql/draft/#InterfaceTypeExtension
data InterfaceTypeExtension
  = InterfaceTypeExtension
    Name
    Directives
    FieldsDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData InterfaceTypeExtension

-- | http://facebook.github.io/graphql/draft/#UnionTypeDefinition
data UnionTypeDefinition
  = UnionTypeDefinition
    (Maybe Description)
    Name
    Directives
    UnionMemberTypes
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData UnionTypeDefinition

-- | http://facebook.github.io/graphql/draft/#UnionMemberTypes
newtype UnionMemberTypes
  = UnionMemberTypes [NamedType]
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)

-- | http://facebook.github.io/graphql/draft/#UnionTypeExtension
data UnionTypeExtension
  = UnionTypeExtension
    Name
    Directives
    UnionMemberTypes
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData UnionTypeExtension

-- | http://facebook.github.io/graphql/draft/#EnumTypeDefinition
data EnumTypeDefinition
  = EnumTypeDefinition
    (Maybe Description)
    Name
    Directives
    EnumValuesDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData EnumTypeDefinition

-- | http://facebook.github.io/graphql/draft/#EnumValuesDefinition
newtype EnumValuesDefinition
  = EnumValuesDefinition [EnumValueDefinition]
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | http://facebook.github.io/graphql/draft/#EnumValueDefinition
data EnumValueDefinition
  = EnumValueDefinition
    (Maybe Description)
    EnumValue
    Directives
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData EnumValueDefinition

-- | http://facebook.github.io/graphql/draft/#EnumTypeExtension
data EnumTypeExtension
  = EnumTypeExtension
    Name
    Directives
    EnumValuesDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData EnumTypeExtension

-- | InputObjectTypeDefinition
-- http://facebook.github.io/graphql/draft/#InputObjectTypeDefinition
data InputObjectTypeDefinition
  = InputObjectTypeDefinition
    (Maybe Description)
    Name
    Directives
    InputFieldsDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData InputObjectTypeDefinition

-- | InputFieldsDefinition
-- http://facebook.github.io/graphql/draft/#InputFieldsDefinition
newtype InputFieldsDefinition
  = InputFieldsDefinition [InputValueDefinition]
  deriving (Show, Eq, Generic, Data, Typeable, NFData)

-- | InputObjectTypeExtension
-- http://facebook.github.io/graphql/draft/#InputObjectTypeExtension
data InputObjectTypeExtension
  = InputObjectTypeExtension Name [Directive] InputFieldsDefinition
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData InputObjectTypeExtension

-- | Directive definition
-- http://facebook.github.io/graphql/draft/#sec-Type-System.Directives
data DirectiveDefinition
  = DirectiveDefinition
    (Maybe Description)
    Name
    ArgumentsDefinition
    DirectiveLocations
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData DirectiveDefinition

-- | http://facebook.github.io/graphql/draft/#DirectiveLocations
type DirectiveLocations = [DirectiveLocation]

-- | http://facebook.github.io/graphql/draft/#DirectiveLocation
data DirectiveLocation
  = LocationExecutableDirective ExecutableDirectiveLocation
  | LocationTypeSystemDirective TypeSystemDirectiveLocation
  deriving (Show, Eq, Generic, Data, Typeable)

instance NFData DirectiveLocation

-- | A GraphQL 'Name'
-- http://facebook.github.io/graphql/draft/#Name
newtype Name = Name Text
  deriving (Show, Eq, Generic, Data, Typeable, Monoid, Semigroup, NFData)
