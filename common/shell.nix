{ project }:

let
  release = (import ./release.nix);
in release.pkgs.stdenv.lib.overrideDerivation release."${project}".env (oldAttrs: rec {
  nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
    release.cabal
    release.pkgs.awscli
    release.pkgs.haskellPackages.cabal2nix
    release.pkgs.haskellPackages.steeloverseer
  ];
})