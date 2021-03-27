{ optimization ? "0", compiler ? "ghc865", reflexPlatform ?
  import ./nix/pkgs.nix {
    inherit compiler;
    reflex = true;
  } }:
let
  # inherit (pkgs.pkgsCross) aarch64-android-prebuilt;
  # armv7a-android-prebuilt
  # inherit (allow-unfree-pkgs.pkgsCross) iphone64 iphone64-simulator;
  # iphone32 iphone32-simulator;

  rp = reflexPlatform.project ({ pkgs, ... }: {
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
