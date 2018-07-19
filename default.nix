{ pkgs ? import <nixpkgs> {}, compiler ? "ghc842" }:
with pkgs.haskell.lib;
  buildStrictly (pkgs.haskell.packages.${compiler}.callPackage ./graphql-meta.nix {})
