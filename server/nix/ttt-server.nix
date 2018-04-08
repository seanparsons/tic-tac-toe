{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "ttt-server";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
