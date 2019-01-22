with import <nixpkgs> {};

let ghcEnv = (pkgs.haskell.packages.ghc863.override { overrides = self: super: {
  # Cabal = self.Cabal_2_4_1_0;
}; }).ghcWithPackages (p: [
  p.Cabal
  p.haddock-library
  p.optparse-applicative
  p.split
  p.utf8-string
  p.microlens
  p.hspec
  p.data-default
  p.ansi-wl-pprint
  p.base-compat
  p.microlens-th
  # p.StrictCheck
  p.generics-sop
  p.hspec-expectations-pretty-diff
]); in

stdenv.mkDerivation {
  NIX_GHC = "${ghcEnv}/bin/ghc";
  NIX_GHCPKG = "${ghcEnv}/bin/ghc-pkg";
  NIX_GHC_DOCDIR = "${ghcEnv}/share/doc/ghc/html";
  NIX_GHC_LIBDIR = "${ghcEnv}/lib/ghc-${ghcEnv.version}";

  name = "shell-env";
  buildInputs = [
    libiconv
    ghcEnv
    pkgs.haskellPackages.cabal-install
    (import (builtins.fetchTarball https://github.com/domenkozar/hie-nix/tarball/master) {}).hies
  ];
}
