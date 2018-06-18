graphql-meta
================

Construct [GraphQL](https://graphql.org/) queries and schema at compile time.

## Table of Contents
- [Query](#query)
  - [Substitution](#substitution)
- [Schema](#schema)
  - [Generics](#generics)
  - [Limitations](#limitations)
- [Maintainers](#maintainers)
- [Credit](#credit)
- [License](#license)

## Query

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import GraphQL.QQ (query)

main :: IO ()
main = print [query|{ building (id: 123) {floorCount, id}}|]
```

#### Result
```haskell
> main

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

### Substitution

[GraphQL](https://graphql.org/) `QueryDocument` abstract syntax tree rewriting is made possible via Haskell's metavariable substitution. During `QuasiQuotation` all unbound variables in a `GraphQL` query that have identical names inside the current Haskell closure will automatically be translated into `GraphQL` AST terms and substituted.

```haskell
buildingQuery
  :: Int
  -> QueryDocument
buildingQuery buildingId =
  [query|{ building (id: $buildingId) {floorCount, id}}|]
```

#### Result

```haskell
> main = print (buildingQuery 4)

QueryDocument {getDefinitions = [
  DefinitionOperation (AnonymousQuery [
    SelectionField (Field Nothing (Name {unName = "building"}) [
      Argument (Name {unName = "buildingId"}) (ValueInt 4)] [] [
	SelectionField (Field Nothing (Name {unName = "floorCount"}) [] [] [])
      , SelectionField (Field Nothing (Name {unName = "id"}) [] [] [])
      ])
    ])
  ]}
```

## Schema

```haskell
{-# LANGUAGE QuasiQuotes #-}

module Main (main) where

import GraphQL.QQ (schema)

main :: IO ()
main = print [schema| type Person { name : String } |]
```

#### Result

```haskell
SchemaDocument [
  TypeDefinitionObject (ObjectTypeDefinition (Name {unName = "Person"}) [] [
    FieldDefinition (Name {unName = "name"}) [] (TypeNamed (NamedType (Name {unName = "String"})))
  ])
]
```

## Generics
It is possible to derive a `SchemaDocument` using `GHC.Generics`.
Simply import `GHC.Generics`, derive `Generic` (must enable the `{-# LANGUAGE DeriveGeneric #-}` language extension, and make an instance of `ToSchemaDocument`. See below for an example:

```haskell
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

main :: IO () = showPersonSchema
-- type Person{name:String!,age:Int!}
```

## Limitations

Generic deriving is only supported on product types with record field selectors.
All generically derived types become `ObjectTypeDefintion`s

## Maintainers

- [@dmjio](https://github.com/dmjio)
- [@rschmuckler](https://github.com/rschmukler)

## Credit

Inspired by [@edsko](https://github.com/edsko)'s work [Quasi-quoting DLS for free](http://www.well-typed.com/blog/2014/10/quasi-quoting-dsls/)


## License

[BSD3](LICENSE) 2018-2019 Urbint Inc.
