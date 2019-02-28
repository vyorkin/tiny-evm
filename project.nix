{ mkDerivation, base, bytestring, co-log, containers, hspec, relude
, stdenv, tasty, tasty-discover, tasty-hspec, text
}:
mkDerivation {
  pname = "tiny-evm";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring co-log containers relude text
  ];
  executableHaskellDepends = [
    base bytestring co-log containers relude text
  ];
  testHaskellDepends = [
    base bytestring co-log containers hspec relude tasty tasty-discover
    tasty-hspec text
  ];
  testToolDepends = [ tasty-discover ];
  description = "A tiny ethereum-like virtual machine";
  license = stdenv.lib.licenses.mit;
}
