{ pkgs ? import ./nix/pkgs.nix { } }:
with pkgs;
dockerTools.buildImage {
  name = "servant-with-beam";
  tag = "lastest";
  contents = [ (import ./default.nix { }).servant-with-beam.components.exes.app busybox ];
  config = { Cmd = [ "bin/app" ]; };
}
