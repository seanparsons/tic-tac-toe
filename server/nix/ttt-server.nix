{ mkDerivation, base, servant-server, stdenv }:
mkDerivation {
  pname = "ttt-server";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base servant-server ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
