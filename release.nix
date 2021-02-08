{ static ? true, pkgs ? import ./nix/pkgs.nix { inherit static; }
, compiler ? "ghc8102" }:
pkgs.dockerTools.buildImage {
  name = "servant-with-beam";
  tag = "latest";
  contents = [
    (import ./default.nix {
      inherit static pkgs compiler;
    }).servant-with-beam.components.exes.app
    pkgs.busybox
  ];
  config = { Cmd = [ "bin/app" ]; };
}
