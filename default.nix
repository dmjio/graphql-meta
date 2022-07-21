{ pkgs ? import <nixpkgs> {} }:
with pkgs.haskell.lib;
  buildStrictly (pkgs.haskellPackages.callPackage ./graphql-meta.nix {})
