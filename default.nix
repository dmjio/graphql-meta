{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./graphql-qq.nix {}
