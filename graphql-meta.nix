{ mkDerivation, alex, array, attoparsec, base, bytestring
, containers, criterion, deepseq, happy, hashable, hspec, mtl
, prettyprinter, QuickCheck, quickcheck-instances, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "graphql-meta";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base bytestring containers deepseq hashable mtl
    prettyprinter template-haskell text
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    base bytestring hspec QuickCheck quickcheck-instances text
  ];
  benchmarkHaskellDepends = [ base bytestring criterion text ];
  homepage = "https://github.com/urbint/graphql-meta";
  description = "Generic and meta programming facilities for GraphQL";
  license = stdenv.lib.licenses.bsd3;
}
