{ mkDerivation, aeson, aeson-pretty, async, base, bytestring
, containers, errors, fetchgit, foldl, optparse-applicative
, process, stdenv, system-filepath, text, turtle
}:
mkDerivation {
  pname = "psc-package";
  version = "0.3.2";
  src = fetchgit {
    url = "git://github.com/purescript/psc-package.git";
    sha256 = "08swc1bjkfyndmhphf210m2cqnq9xlz8brvll3xxk2adx6ylixzk";
    rev = "43891df755d7af1e2d87a4c05c8e641b6eefe1e9";
  };
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson aeson-pretty async base bytestring containers errors foldl
    optparse-applicative process system-filepath text turtle
  ];
  homepage = "https://github.com/purescript/psc-package";
  description = "An experimental package manager for PureScript";
  license = stdenv.lib.licenses.bsd3;
}
