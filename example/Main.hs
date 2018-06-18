{-# LANGUAGE DeriveGeneric    #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes      #-}
--------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : Example usage of graphql-meta
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
module Main (main) where
--------------------------------------------------------------------------------
import           GHC.Generics
import           GraphQL.Internal.Syntax.AST
import           GraphQL.Internal.Syntax.Encoder
import qualified Data.Text.IO                    as T
import           Data.Proxy
--------------------------------------------------------------------------------
import           GraphQL.QQ
import           GraphQL.Generic
--------------------------------------------------------------------------------

main :: IO ()
main = do
  print rawSchema
  print rawQuery
  print (substitutedQuery "foo")
  showPersonSchema

-- | Example of parsing a raw schema
rawSchema :: SchemaDocument
rawSchema = [schema|
  type Person {
    name: String
  }
|]

-- | In this case, `x` is *not* in scope, so not substitution is performed
rawQuery :: QueryDocument
rawQuery = [query|{
  human(id: $x) {
    name
    height
  }
}|]

-- | In this case, `x` is in scope, so substitution is performed
substitutedQuery :: ToExpr x => x -> QueryDocument
substitutedQuery x = [query|{
  human(id: $x) {
    name
    height
  }
}|]

-- | Example of using GHC.Generics to create a Schema from a product type
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic)

instance ToSchemaDocument Person

showPersonSchema :: IO ()
showPersonSchema
  = T.putStrLn
  $ schemaDocument
  $ toSchemaDocument (Proxy @ Person)
