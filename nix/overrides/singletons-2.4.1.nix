{ mkDerivation, base, containers, directory, filepath, ghc-boot-th
, mtl, process, stdenv, syb, tasty, tasty-golden, template-haskell
, text, th-desugar, transformers
}:
mkDerivation {
  pname = "singletons";
  version = "2.4.1";
  sha256 = "5d7a200c814a5f1ac16db04456fdafbdea39fc0ee6c934a9ef7bcd2d6da2f9cf";
  libraryHaskellDepends = [
    base containers ghc-boot-th mtl syb template-haskell text
    th-desugar transformers
  ];
  testHaskellDepends = [
    base directory filepath process tasty tasty-golden
  ];
  homepage = "http://www.github.com/goldfirere/singletons";
  description = "A framework for generating singleton types";
  license = stdenv.lib.licenses.bsd3;
}
