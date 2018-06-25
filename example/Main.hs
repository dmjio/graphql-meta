{-# LANGUAGE OverloadedStrings #-}
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
import qualified GraphQL.Internal.Syntax.Parser as P
import qualified Data.Text.IO                    as T
import           Data.Proxy
--------------------------------------------------------------------------------
import           GraphQL.QQ
import           GraphQL.Generic
--------------------------------------------------------------------------------

main :: IO ()
main = do
  let k = foo ("asdlf" :: String)
  print k
  T.putStrLn $ queryDocument k
  T.putStrLn $ queryDocument supG

-- λ> main
-- {building(param:"uuid-goes-here"){floorCount,height}}

supG :: QueryDocument
supG = substitutedQuery "building" "uuid-goes-here" "floorCount" "height"

substitutedQuery
  :: String
  -> String
  -> String
  -> String
  -> QueryDocument
substitutedQuery a b c d
  = [query| { a(param: $b) { c d } } |]

foo :: String -> QueryDocument
foo hi = [query| query WhoHah ($id: String!) { building(id: $hi) { floorCount } }|]

-- | Example of using GHC.Generics to create a Schema from a product type
data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic)

instance ToObjectTypeDefinition Person

showPersonSchema :: IO ()
showPersonSchema
  = T.putStrLn
  $ schemaDocument
  $ SchemaDocument [ TypeDefinitionObject obj ]
    where
      obj = toObjectTypeDefinition (Proxy @ Person)
