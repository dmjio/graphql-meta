{ pkgs ? import <nixpkgs> {} }:
  pkgs.haskellPackages.callPackage ./graphql-meta.nix {}
