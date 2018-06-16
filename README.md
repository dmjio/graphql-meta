graphql-qq
================

Construct GraphQL queries and schema at compile time

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import GraphQL.QQ (query)

main :: IO ()
main = print [query|{ building (id: 123) {floorCount, id}}|]
```

## Result
```haskell
λ> main
QueryDocument {getDefinitions = [
  DefinitionOperation (AnonymousQuery [
    SelectionField (Field Nothing (Name {unName = "building"}) [
      Argument (Name {unName = "id"}) (ValueInt 123)] [] [
	SelectionField (Field Nothing (Name {unName = "floorCount"}) [] [] [])
      , SelectionField (Field Nothing (Name {unName = "id"}) [] [] [])
      ])
    ])
  ]}
```

## Roadmap
  - Variable interpolation
