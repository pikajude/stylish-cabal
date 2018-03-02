{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:

with nixpkgs.pkgs; stdenv.mkDerivation {
  name = "shellenv";
  buildInputs = [
    zlib
    haskell.compiler."${compiler}"
  ];
}
