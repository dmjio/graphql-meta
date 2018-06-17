{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : GraphQL.Generic
-- Description : Compile-time facilities for dealing with GraphQL schema / queries
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module GraphQL.Generic
  ( -- * QuasiQuoters
    ToSchemaDocument (..)
  ) where
--------------------------------------------------------------------------------
import           GHC.Generics
import           Data.Proxy
import           Data.Monoid
--------------------------------------------------------------------------------
import qualified GraphQL.Internal.Name          as GQLName
import           GraphQL.Internal.Syntax.AST
import           GraphQL.Internal.Syntax.Parser
--------------------------------------------------------------------------------
-- | Generically convert GraphQL Schema into `SchemaDocument`
class ToSchemaDocument (a :: *) where
  toSchemaDocument
    :: Proxy a
    -> SchemaDocument
  default toSchemaDocument
    :: (Generic a, GToSchemaDocument (Rep a))
    => Proxy a
    -> SchemaDocument
  toSchemaDocument Proxy =
     SchemaDocument
       $ gToSchemaDocument
       $ from @a undefined

class GToSchemaDocument f where
  gToSchemaDocument
    :: f a
    -> [TypeDefinition]

instance (GToSchemaDocument a, GToSchemaDocument b) =>
  GToSchemaDocument (a :*: b) where
    gToSchemaDocument (a :*: b) =
      gToSchemaDocument a <> gToSchemaDocument b

instance (GToSchemaDocument a, GToSchemaDocument b) =>
  GToSchemaDocument (a :+: b) where
    gToSchemaDocument (L1 l) = gToSchemaDocument l
    gToSchemaDocument (R1 r) = gToSchemaDocument r
