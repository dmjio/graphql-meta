{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import GraphQL.QQ
import GraphQL.Pretty
import GraphQL.Parser

import Criterion.Main

someQuery :: String
someQuery = show $ printExecutableDefinition [query|
query HeroNameAndFriends($episode: Episode, $foobar : String! ) {
  hero(episode: $episode, wozloz: $foobar) {
    name
    friends {
      name {
        happy @lol @cool
      }
    }
   name
    friends {
      name
    }
   name
    friends {
      name
    }
   name
    friends {
      name
    }
  }
}
|]

main :: IO ()
main = defaultMain [
  bgroup "bench" [ bench "Parse graphQL query" (nf parseDocument someQuery) ]
  ]
