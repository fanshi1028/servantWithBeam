{ useWarp ? false, pkgSets ? import ./nix/pkgs.nix { } }:
let inherit (pkgSets) reflexProject;
in reflexProject ({ pkgs, ... }: {
  inherit useWarp;
  packages = { frontend = ./frontend; };
  shells = {
    ghc = [ "frontend" ];
    ghcjs = [ "frontend" ];
  };
  android.frontend = {
    executableName = "frontend";
    applicationId = "servant.with.beam.frontend";
    displayName = "Servant With Beam App";
  };
  ios.frontend = {
    executableName = "frontend";
    applicationId = "servant.with.beam.frontend";
    displayName = "Servant With Beam App";
  };
})
