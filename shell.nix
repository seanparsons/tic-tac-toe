{ compiler ? "ghc822" }:

let
  release = (import ./server/release.nix {inherit compiler;});
  pkgs = release.pkgs;
in pkgs.stdenv.lib.overrideDerivation release.ttt-server.env (oldAttrs: rec {
  nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
    release.cabal
    pkgs.zlib.dev
    pkgs.zlib.out
    pkgs.awscli
    pkgs.haskellPackages.cabal2nix
    pkgs.haskellPackages.purescript
    (pkgs.haskell.packages.${compiler}.callPackage ./psc-package.nix {})
  ];
})
