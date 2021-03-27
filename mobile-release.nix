{ optimization ? "0", compiler ? "ghc865"
, reflexPlatform ? (import ./nix/pkgs.nix { inherit compiler; reflex = true; }).reflex }:
let
  # inherit (pkgs.pkgsCross) aarch64-android-prebuilt;
  # armv7a-android-prebuilt
  # inherit (allow-unfree-pkgs.pkgsCross) iphone64 iphone64-simulator;
  # iphone32 iphone32-simulator;

  rp = (reflexPlatform {
    config = {
      android_sdk.accept_license = true;
      # allowBroken = true;
    };

    # haskellOverlaysPost = [
    #   (self: super: {
    #     servant-with-beam =
    #       pkgs.haskellPackages.callCabal2nixWithOptions "servant-with-beam"
    #       ./. "-ffrontend" { };
    #     # reflex-dom in reflex-platform was created by callCabal2nix which seems to be not respecting os conditional in its cabal, and cause dependency issue
    #     # https://github.com/reflex-frp/reflex-platform/blob/f019863c21ee85498e6a6e0072e617b2462b70ed/haskell-overlays/reflex-packages/default.nix#L83
    #     reflex-dom = pkgs.haskellPackages.lib.overrideCabal super.reflex-dom
    #       (drv: {
    #         # Hack until https://github.com/NixOS/cabal2nix/pull/432 lands
    #         libraryHaskellDepends = (drv.libraryHaskellDepends or [ ])
    #           ++ pkgs.lib.optionals pkgs.hostPlatform.isAndroid [
    #             self.android-activity
    #             self.aeson
    #             self.data-default
    #             self.jsaddle
    #           ] ++ pkgs.lib.optionals pkgs.hostPlatform.isIos [
    #             self.data-default
    #             self.jsaddle
    #             self.jsaddle-wkwebview
    #           ];
    #       });
    #   })
    # ] ;
  }).project ({ pkgs, ... }: {
    packages = { servant-with-beam-frontend = ./frontend; };
    android.servant-with-beam-frontend = {
      executableName = "frontend";
      applicationId = "my.frontend";
      displayName = "Android App";
      # github action run out of space, try build only this to see if it helps
      abiVersions = [ "arm64-v8a" ];
    };
    ios.servant-with-beam-frontend = {
      executableName = "frontend";
      bundleIdentifier = "my.frontend";
      bundleName = "IOS App";
    };
    overrides = self: super: {
      universum = pkgs.haskell.lib.dontCheck super.universum;
    };
  });
in {
  android = rp.android.servant-with-beam-frontend;
  ios = rp.ios.servant-with-beam-frontend;
}
#                     // {
#   android = (rp aarch64-android-prebuilt).android.buildApp ({
#     package = p: p.servant-with-beam;
#     executableName = "frontend";
#     applicationId = "my.frontend";
#     displayName = "Android App";
#   });
#   ios = (rp iphone64).ios.buildApp ({
#     package = p: p.servant-with-beam;
#     executableName = "frontend";
#     bundleIdentifier = "my.frontend";
#     bundleName = "IOS App";
#   });
# };
