{ mkDerivation, base, mwc-probability, mwc-random, primitive, repa
, stdenv
}:
mkDerivation {
  pname = "bart";
  version = "0.1.0.0";
  src = ./.;
  libraryHaskellDepends = [
    base mwc-probability mwc-random primitive repa
  ];
  homepage = "https://github.com/charlieshanley/bart";
  description = "Bayesian additive regression trees";
  license = stdenv.lib.licenses.bsd3;
}
