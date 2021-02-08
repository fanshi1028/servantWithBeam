{ static ? true, pkgs ? import ./nix/pkgs.nix { inherit static; } }:
pkgs.dockerTools.buildImage {
  name = "servant-with-beam";
  tag = "latest";
  contents = [
    (import ./default.nix {
      inherit static pkgs;
    }).servant-with-beam.components.exes.app
    pkgs.busybox
  ];
  config = { Cmd = [ "bin/app" ]; };
}
