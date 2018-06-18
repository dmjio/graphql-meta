{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE DataKinds             #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.Generic
-- Description : Compile-time facilities for dealing with GraphQL schema / queries
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.Generic
  ( -- * Classes
    ToSchemaDocument  (..)
  , GToSchemaDocument (..)
  , ToNamed           (..)
  , ToGQLType         (..)
  ) where
--------------------------------------------------------------------------------
import           GHC.Generics
import           GHC.TypeLits
import           Data.Proxy
import           Data.Text                       (Text, pack)
import qualified Data.Text.IO                    as T
import           Data.Monoid
import           GraphQL.Internal.Syntax.Encoder
--------------------------------------------------------------------------------
import           GraphQL.Internal.Name
import           GraphQL.Internal.Syntax.AST
--------------------------------------------------------------------------------

-- | Generically convert any product type into a 'SchemaDocument'
class ToSchemaDocument (a :: *) where
  toSchemaDocument
    :: Proxy a
    -> SchemaDocument
  default toSchemaDocument
    :: (Generic a, GToSchemaDocument (Rep a))
    => Proxy a
    -> SchemaDocument
  toSchemaDocument Proxy
     = SchemaDocument
     $ pure
     $ TypeDefinitionObject
     $ flip gToSchemaDocument emptyObjectTypeDef
     $ Proxy @ (Rep a)

class GToSchemaDocument (f :: * -> *) where
  gToSchemaDocument
    :: Proxy f
    -> ObjectTypeDefinition
    -> ObjectTypeDefinition

-- | Empty Object type
emptyObjectTypeDef :: ObjectTypeDefinition
emptyObjectTypeDef =
  ObjectTypeDefinition
    (Name mempty)
    mempty
    mempty

addName
  :: Text
  -> ObjectTypeDefinition
  -> ObjectTypeDefinition
addName name (ObjectTypeDefinition _ _ fields)
  = ObjectTypeDefinition (Name name) [] fields

addField
  :: FieldDefinition
  -> ObjectTypeDefinition
  -> ObjectTypeDefinition
addField field (ObjectTypeDefinition name _ fields)
  = ObjectTypeDefinition name [] (field:fields)

combineFields
  :: ObjectTypeDefinition
  -> ObjectTypeDefinition
  -> ObjectTypeDefinition
combineFields
  (ObjectTypeDefinition _ _ as)
  (ObjectTypeDefinition name _ bs)
  = ObjectTypeDefinition name [] (as <> bs)

instance GToSchemaDocument a => GToSchemaDocument (D1 i a) where
  gToSchemaDocument Proxy = gToSchemaDocument (Proxy @ a)

instance (KnownSymbol name, GToSchemaDocument a) =>
  GToSchemaDocument (C1 (MetaCons name x y) a) where
    gToSchemaDocument Proxy obj =
      gToSchemaDocument (Proxy @ a) (addName name obj)
        where
          name = pack $ symbolVal (Proxy @ name)

instance (ToGQLType gType, KnownSymbol name) =>
  GToSchemaDocument (S1 (MetaSel (Just name) u s d) (K1 i gType)) where
    gToSchemaDocument Proxy = addField field
        where
          field = FieldDefinition fName [] gtype
          fName = Name $ pack $ symbolVal (Proxy @ name)
          gtype = toGQLType (Proxy @ gType)

instance GToSchemaDocument U1 where
  gToSchemaDocument Proxy = id

instance (GToSchemaDocument a, GToSchemaDocument b) =>
  GToSchemaDocument (a :*: b) where
    gToSchemaDocument Proxy o =
      gToSchemaDocument (Proxy @ a) o
        `combineFields`
          gToSchemaDocument (Proxy @ b) o

instance (GToSchemaDocument a, GToSchemaDocument b) =>
  GToSchemaDocument (a :+: b) where
    gToSchemaDocument Proxy o =
      gToSchemaDocument (Proxy @ a) o
        `combineFields`
           gToSchemaDocument (Proxy @ b) o

class ToNamed a where
  toNamed :: Proxy a -> NamedType

instance ToNamed String where
  toNamed Proxy = NamedType (Name "String")

instance ToNamed Double where
  toNamed Proxy = NamedType (Name "Float")

instance ToNamed Text where
  toNamed Proxy = NamedType (Name "String")

instance ToNamed Int where
  toNamed Proxy = NamedType (Name "Int")

instance ToNamed Integer where
  toNamed Proxy = NamedType (Name "Int")

instance ToNamed Bool where
  toNamed Proxy = NamedType (Name "Boolean")

class ToGQLType a where
  toGQLType :: Proxy a -> GType

instance ToGQLType a => ToGQLType [a] where
  toGQLType Proxy
    = TypeNonNull
    $ NonNullTypeList
    $ ListType
    $ toGQLType (Proxy @ a)

instance {-# overlaps #-} ToGQLType String where
  toGQLType Proxy
    = TypeNonNull
    $ NonNullTypeNamed
    $ toNamed (Proxy @ String)

instance ToGQLType Int where
  toGQLType Proxy
    = TypeNonNull
    $ NonNullTypeNamed
    $ toNamed (Proxy @ Int)

instance ToGQLType Integer where
  toGQLType Proxy
    = TypeNonNull
    $ NonNullTypeNamed
    $ toNamed (Proxy @ Integer)

instance ToGQLType Double where
  toGQLType Proxy
    = TypeNonNull
    $ NonNullTypeNamed
    $ toNamed (Proxy @ Double)

instance ToGQLType Bool where
  toGQLType Proxy
    = TypeNonNull
    $ NonNullTypeNamed
    $ toNamed (Proxy @ Bool)

instance ToNamed a => ToGQLType (Maybe a) where
  toGQLType Proxy = TypeNamed $ toNamed (Proxy @ a)
