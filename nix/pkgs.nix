{ sources ? import ./sources.nix }:
let
  reflexProject = (import sources.reflex-platform {
    # nixpkgsFunc = import nixpkgsSrc;
    config.android_sdk.accept_license = true;
    # haskellOverlaysPost = []
  }).project;

in { inherit reflexProject; }
