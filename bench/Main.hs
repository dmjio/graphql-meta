{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Criterion.Main
import Data.ByteString    (ByteString)
import Data.Text          (pack)
import Data.Text.Encoding (encodeUtf8)

import GraphQL.Parser
import GraphQL.Pretty
import GraphQL.QQ

someQuery :: ByteString
someQuery = encodeUtf8 $ pack $ show $ printExecutableDefinition [query|
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
