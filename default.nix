{ pkgs ? import <nixpkgs> {}, compiler ? "ghc843" }:
with pkgs.haskell.lib;
  buildStrictly (pkgs.haskell.packages.${compiler}.callPackage ./graphql-meta.nix {})
