{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", test ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};

  drv = with pkgs.haskell.lib; (if test then enableCabalFlag else disableCabalFlag)
    ((haskellPackages.callCabal2nix "stylish-cabal" ./. {})
      .overrideScope (self: super: { Cabal = self.Cabal_2_0_1_1; }))
    "test-hackage";

in

  if pkgs.lib.inNixShell then drv.env else drv
