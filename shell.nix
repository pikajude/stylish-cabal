{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, async, base, Cabal_2_0_0_2, containers, hlint
      , hspec, hspec-expectations-pretty-diff, HUnit, lens, mtl
      , optparse-applicative, split, stdenv, temporary, utf8-string
      , wl-pprint, wreq
      }:
      mkDerivation {
        pname = "stylish-cabal";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [ base Cabal_2_0_0_2 mtl split wl-pprint ];
        executableHaskellDepends = [ base optparse-applicative wl-pprint ];
        testHaskellDepends = [
          aeson async base Cabal_2_0_0_2 containers hlint hspec
          hspec-expectations-pretty-diff HUnit lens temporary utf8-string
          wreq
        ];
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
