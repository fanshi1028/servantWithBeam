{ compiler ? "ghc8104", platform ? "linux", optimization ? "2", default ? false
, pkgs ? import ./default.nix {
  inherit compiler platform default checkMaterialization optimization;
}, checkMaterialization ? false }:
with pkgs;
dockerTools.buildImage {
  name = "servant-with-beam";
  tag = "latest";
  contents = [ servant-with-beam."${platform}" busybox ];
  config = { Cmd = [ "bin/app" ]; };
}
