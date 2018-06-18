{ mkDerivation, attoparsec, base, containers, graphql-api, stdenv
, template-haskell, text
}:
mkDerivation {
  pname = "graphql-meta";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base containers graphql-api template-haskell text
  ];
  executableHaskellDepends = [ base graphql-api text ];
  homepage = "https://github.com/urbint/graphql-meta";
  description = "Generic and meta programming facilities for GraphQL";
  license = stdenv.lib.licenses.bsd3;
}
