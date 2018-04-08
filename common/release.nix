let
  projectCompilers = {
    server = "ghc802";
    frontend = "ghcjsHEAD";
  };
  projectCompiler = project: projectCompilers."${project}";

  # Disable tests for these packages
  dontCheckPackages = [
  ];

  # Jailbreak these packages
  doJailbreakPackages = [
  ];

  # Disable haddocks for these packages
  dontHaddockPackages = [
  ];

  generatedOverrides = project: haskellPackagesNew: haskellPackagesOld:
    let
      toPackage = file: _: {
        name  = builtins.replaceStrings [ ".nix" ] [ "" ] file;
        value = haskellPackagesNew.callPackage (../. + "/${project}/nix/${file}") { };
      };
    in
      pkgs.lib.mapAttrs' toPackage (builtins.readDir (../. + "/${project}/nix"));

  makeOverrides =
    function: names: haskellPackagesNew: haskellPackagesOld:
      let
        toPackage = name: {
          inherit name;
          value = function haskellPackagesOld.${name};
        };
      in
        builtins.listToAttrs (map toPackage names);

  composeExtensionsList = pkgs.lib.fold pkgs.lib.composeExtensions (_: _: {});

  # More exotic overrides go here
  manualOverrides = haskellPackagesNew: haskellPackagesOld: {
  };

  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.lib.foldl' (working: project: working // {
          "${projectCompiler project}" = pkgs.haskell.packages."${projectCompiler project}".override {
            overrides = composeExtensionsList [
              (generatedOverrides project)
              (makeOverrides pkgs.haskell.lib.dontCheck   dontCheckPackages  )
              (makeOverrides pkgs.haskell.lib.doJailbreak doJailbreakPackages)
              (makeOverrides pkgs.haskell.lib.dontHaddock dontHaddockPackages)
              manualOverrides
            ];
          };
        }) pkgs.haskell.packages (pkgs.lib.attrNames projectCompilers);
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };

in
  { ttt-server = pkgs.haskell.packages.${projectCompilers.server}.ttt-server;
    ttt-frontend = pkgs.haskell.packages.${projectCompilers.frontend}.ttt-frontend;
    cabal = pkgs.haskellPackages.cabal-install;
    pkgs = pkgs;
  }