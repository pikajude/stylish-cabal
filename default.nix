{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", test ? false }:

let

  inherit (nixpkgs) pkgs;

  haskellPackages = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages.${compiler};

  drv = with pkgs.haskell.lib; overrideCabal (
    (haskellPackages.callCabal2nix "stylish-cabal" ./. {})
      .overrideScope (self: super: { Cabal = null; }))
    (drv: pkgs.lib.optionalAttrs test {
      pname = drv.pname + "-${compiler}";
      configureFlags = [ "-ftest-strictness" "-fwerror" ];
    });

in

  if pkgs.lib.inNixShell then drv.env else drv
