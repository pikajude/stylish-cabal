{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = (haskellPackages.callCabal2nix "stylish-cabal" ./. {}).overrideScope (
    self: super: { Cabal = self.Cabal_2_0_0_2; }
  );

in

  if pkgs.lib.inNixShell then drv.env else drv
