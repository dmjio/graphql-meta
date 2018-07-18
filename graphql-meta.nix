{ mkDerivation, alex, array, attoparsec, base, containers, happy
, hspec, prettyprinter, QuickCheck, quickcheck-instances, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "graphql-meta";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base containers prettyprinter template-haskell
    text
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    base hspec QuickCheck quickcheck-instances text
  ];
  homepage = "https://github.com/urbint/graphql-meta";
  description = "Generic and meta programming facilities for GraphQL";
  license = stdenv.lib.licenses.bsd3;
}
