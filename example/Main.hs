{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
module Main (main) where
--------------------------------------------------------------------------------
-- |
-- Module      : Main
-- Description : Example usage of graphql-qq
-- Maintainer  : David Johnson <david@urbint.com>, Ryan Schmukler <ryan@urbint.com>
-- Maturity    : Usable
--
--------------------------------------------------------------------------------
import GraphQL.QQ
import GraphQL.Internal.Syntax.AST
--------------------------------------------------------------------------------

main :: IO ()
main = do
  print rawSchema
  print rawQuery
  print (substitutedQuery "foo")

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
