{ sources ? import ./sources.nix }:
let
  haskellNix = import sources.haskell-nix { };
  nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;
  reflexProject = (import sources.reflex-platform {
    nixpkgsFunc = import nixpkgsSrc;
    config.android_sdk.accept_license = true;
    # haskellOverlaysPost = []
  }).project;

in { inherit reflexProject; }
