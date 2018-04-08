{ mkDerivation, base, containers, lens, miso, stdenv }:
mkDerivation {
  pname = "ttt-frontend";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base containers lens miso ];
  license = stdenv.lib.licenses.mit;
}
