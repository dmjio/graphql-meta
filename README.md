graphql-meta
================

A [GraphQL](https://graphql.org/) toolkit providing the following:
  - Alex [Lexer](http://facebook.github.io/graphql/draft/#sec-Appendix-Grammar-Summary.Lexical-Tokens) of the `GraphQL` lexical specification.
    - [Source](https://github.com/urbint/graphql-meta/blob/master/src/GraphQL/Lexer.x)
  - Happy [Parser](http://facebook.github.io/graphql/draft/#sec-Appendix-Grammar-Summary.Document) of the `GraphQL` BNF Grammar.
    - [Source](https://github.com/urbint/graphql-meta/blob/master/src/GraphQL/Lexer.x)
  - [Pretty printer](http://hackage.haskell.org/package/prettyprinter) of the `GraphQL` abstract syntax tree (AST) for human consumption.
    - [Source](https://github.com/urbint/graphql-meta/blob/master/src/GraphQL/Pretty.hs)
  - [QuickCheck](http://www.cse.chalmers.se/~rjmh/QuickCheck/manual.html) generators for creating random AST fragments
    - Used in conjunction with pretty printing to establish round trip property tests.
	- [Source](https://github.com/urbint/graphql-meta/blob/master/test/Test/GraphQL/Gen.hs)
  - Generics implementation providing correct-by-construction `Schema` at compile time.
    - [Source](https://github.com/urbint/graphql-meta/blob/master/src/GraphQL/Generic.hs)
  - QuasiQuoter providing inline definitions of `ExecutableDefinitions`.
    - [Source](https://github.com/urbint/graphql-meta/blob/master/src/GraphQL/QQ.hs)

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
main = print [query| { building (id: 123) {floorCount, id}} |]
```

#### Result

```haskell
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

[GraphQL](https://graphql.org/) `ExecutableDefinition` abstract syntax tree rewriting is made possible via Template Haskell's metavariable substitution. During `QuasiQuotation` all unbound variables in a `GraphQL` query that have identical names inside the current scope will automatically be translated into `GraphQL` AST terms and substituted.

```haskell
buildingQuery
  :: Int
  -> ExecutableDefinition
buildingQuery buildingId =
  [query| { building (id: $buildingId) {floorCount, id}} |]
```

#### Result

```haskell
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

### Generics

It is possible to derive GraphQL schema using `GHC.Generics`.
Simply import `GHC.Generics`, derive `Generic` (must enable the `DeriveGeneric` language extension) and make an instance of `ToObjectTypeDefintion`.
See below for an example:

```haskell
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           GHC.Generics                    (Generic)
import           GraphQL.Internal.Syntax.Encoder (schemaDocument)
import           Data.Proxy                      (Proxy)
import qualified Data.Text.IO                    as T
import           GraphQL.Generic                 (ToObjectTypeDefinition(..))

data Person = Person
  { name :: String
  , age  :: Int
  } deriving (Show, Eq, Generic)

instance ToObjectTypeDefinition Person

showPersonSchema :: IO ()
showPersonSchema = print $ toObjectTypeDefinition (Proxy @ Person)

-- type Person{name:String!,age:Int!}
```

## Limitations

- Generic deriving is currently only supported on product types with record field selectors.
- Only `ObjectTypeDefintion` is currently supported.

## Roadmap

- Generic deriving of `ScalarTypeDefintion` and `EnumTypeDefintion`.

## Maintainers

- [@dmjio](https://github.com/dmjio)
- [@rschmuckler](https://github.com/rschmukler)

## Credit

- Inspired by [@edsko](https://github.com/edsko)'s work [Quasi-quoting DLS for free](http://www.well-typed.com/blog/2014/10/quasi-quoting-dsls/).
- `Alex` and `Happy` lexing & parsing inspired by [config-value](https://github.com/glguy/config-value)

## License

[BSD3](LICENSE) 2018-2019 Urbint Inc.
