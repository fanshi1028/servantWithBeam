{ js ? false, frontend ? js, compiler ? if js then "ghc865" else "ghc8104"
, platform ? "linux", optimization ? "2", default ? false
, checkMaterialization ? false, pkgs ? import ./default.nix {
  inherit compiler platform default checkMaterialization optimization js
    frontend;
} }:
with pkgs;
let app = servant-with-beam."${platform}";
in if (js) then
  app
else
  pkgSet.dockerTools.buildImage {
    name = "servant-with-beam";
    tag = "latest";
    contents = [ app pkgSet.busybox ];
    config = { Cmd = [ "bin/app" ]; };
  }
