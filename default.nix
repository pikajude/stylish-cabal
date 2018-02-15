{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = (haskellPackages.callCabal2nix "stylish-cabal" ./. {})
    .overrideScope (self: super: {
      # Cabal = self.Cabal_2_3_0_0;
      # lens = pkgs.haskell.lib.overrideCabal super.lens (drv: {
      #   # custom setup needs Cabal == 2.0.*
      #   postPatch = ''
      #     rm Setup.lhs
      #   '';
      # });
    });

in

  if pkgs.lib.inNixShell then drv.env else drv
