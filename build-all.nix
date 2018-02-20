with import <nixpkgs> { system = "x86_64-linux"; };

# build on linux since hydra has a lot of GHC versions cached for it. they can't be relied
# on to build with newer darwin stdenvs

let source = pkgs.runCommand "src" {
        buildInputs = [ pkgs.haskellPackages.cabal-install ];
    } ''
        # cabal sdist roundtrip to ensure we push a buildable tarball to hackage
        mkdir -p $out unpack
        export HOME=$TMP # cabal tries to create ~/.cabal
        unpackFile ${./.}
        cd stylish-cabal
        cabal sdist --output-directory=$out

        # not part of sdist for obvious reasons
        cp ${./default.nix} $out/default.nix
    '';
in builtins.map
(compiler: callPackage source { inherit compiler; nixpkgs = pkgs; test = true; })
    [ /* "ghc763" */ "ghc784" "ghc7103" "ghc802" "ghc822" "ghc841" ]
