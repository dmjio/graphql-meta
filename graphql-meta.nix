{ mkDerivation, alex, array, attoparsec, base, containers
, criterion, deepseq, happy, hspec, prettyprinter, QuickCheck
, quickcheck-instances, stdenv, template-haskell, text
}:
mkDerivation {
  pname = "graphql-meta";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array attoparsec base containers deepseq prettyprinter
    template-haskell text criterion
  ];
  libraryToolDepends = [ alex happy ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    base hspec QuickCheck quickcheck-instances text
  ];
  benchmarkHaskellDepends = [ base criterion ];
  homepage = "https://github.com/urbint/graphql-meta";
  description = "Generic and meta programming facilities for GraphQL";
  license = stdenv.lib.licenses.bsd3;
}
