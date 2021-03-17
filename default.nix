{ useWarp ? false, pkgSets ? import ./nix/pkgs.nix { inherit useWarp; } }:
let inherit (pkgSets) reflexProject;
in reflexProject ({ pkgs, ... }: {
  packages = { servant-with-beam-frontend = ./frontend; };
  shells = {
    ghc = [ "servant-with-beam-frontend" ];
    ghcjs = [ "servant-with-beam-frontend" ];
  };
  android.frontend = {
    executableName = "servant-with-beam-frontend";
    applicationId = "servant.with.beam.frontend";
    displayName = "Servant With Beam App";
  };
  ios.frontend = {
    executableName = "servant-with-beam-frontend";
    applicationId = "servant.with.beam.frontend";
    displayName = "Servant With Beam App";
  };
})
