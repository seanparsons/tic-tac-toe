{ mkDerivation, aeson, base, blaze-html, blaze-svg, clay, extra
, hashable, lens, mtl, servant-blaze, servant-server, stdenv, text
, unordered-containers, uuid, wai, warp
}:
mkDerivation {
  pname = "ttt-server";
  version = "0.1.0.0";
  src = ./..;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson base blaze-html blaze-svg clay extra hashable lens mtl
    servant-blaze servant-server text unordered-containers uuid wai
    warp
  ];
  license = stdenv.lib.licenses.unfree;
  hydraPlatforms = stdenv.lib.platforms.none;
}
