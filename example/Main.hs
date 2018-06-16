{-# LANGUAGE QuasiQuotes #-}
module Main (main) where

import GraphQL.QQ (query)

main :: IO ()
main = print [query|{ building (id: 123) {floorCount, id}}|]
