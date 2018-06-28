{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Main where

import GraphQL.QQ

main :: IO ()
main = print [query|
query GetBuildings($ids: Identifier = 123, $limit: Int = 500)  {
  building_many(ids: $ids, limit: $limit) {
    total_count
    failed_lookups
    results {
      idk, floorCount
    }
  }
}
|]
