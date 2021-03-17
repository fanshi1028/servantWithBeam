{ useWarp ? false, sources ? import ./sources.nix }:
let
  reflexProject = (import sources.reflex-platform {
    inherit useWarp;
    # nixpkgsFunc = import nixpkgsSrc;
    config.android_sdk.accept_license = true;
    # haskellOverlaysPost = []
  }).project;

in { inherit reflexProject; }
