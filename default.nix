{ js ? false, optimization ? "0", frontend ? js
, compiler ? if frontend then "ghc865" else "ghc8104", platform ? "osx"
, default ? true, pkgSets ? import ./nix/pkgs.nix { inherit compiler; }
, checkMaterialization ? false, useWarp ? false, sha256 ? "" }:
let
  inherit (pkgSets)
    pkgs static-pkgs win64-pkgs allow-unfree-pkgs reflexPlatform obNixpkgsFunc;
  # NOTE https://github.com/input-output-hk/haskell.nix/issues/276#issue-512788094
  inherit (win64-pkgs.pkgsCross) mingwW64;
  # inherit (pkgs.pkgsCross) mingwW64 musl64;
  inherit (pkgs.lib.attrsets) mapAttrs;
  inherit (pkgs.pkgsCross) aarch64-android-prebuilt;
  # armv7a-android-prebuilt
  inherit (allow-unfree-pkgs.pkgsCross) iphone64 iphone64-simulator;
  # iphone32 iphone32-simulator;

  # NOTE https://github.com/input-output-hk/haskell.nix/issues/864#issuecomment-702971226
  backendFiles = [ "backend" ];
  frontendFiles = [ "frontend" ];

  name = "servant-with-beam";
  baseSrc = ./.;

  compiler-nix-name = compiler;

  mkProject = raw-pkgs:
    # this sha256 is not used
    raw-sha256:
    let
      pkgs = if (js) then raw-pkgs.pkgsCross.ghcjs else raw-pkgs;
      # plan-sha256 = if (sha256 == "") then raw-sha256 else sha256;
      plan-sha256 = if (sha256 == "") then null else sha256;
      inherit (pkgs.haskell-nix) haskellLib project;
      inherit (pkgs.lib) any strings optional attrsets readFile;
      # 'cleanGit' cleans a source directory based on the files known by git
      # NOTE https://github.com/input-output-hk/haskell.nix/issues/492
      # NOTE cleanGit not lorri friendly
      # NOTE use less good nix-gitignore instead for now
      # src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
      # NOTE https://github.com/input-output-hk/haskell.nix/issues/1013
      ignore = path:
        baseNameOf path != ".envrc" || baseNameOf path != "shell.nix"
        || baseNameOf path != ".ghcid" || baseNameOf path != "servant-with-beam-frontend.cabal";
      filter = path: type:
        ignore path && any (f:
          let p = toString (baseSrc + ("/" + f));
          in p == path || (strings.hasPrefix (p + "/") path))
        (if (frontend) then frontendFiles else backendFiles) || baseNameOf path
        == "${name}.cabal";
      src = haskellLib.cleanSourceWith {
        inherit name filter;
        src = baseSrc;
      };
      cabalProjectFile = "cabal.project${
          if useWarp then
            ".useWarp"
          else if js then
            (if compiler != "ghc865" then ".frontend.88up" else ".frontend")
          else if frontend then
            (if pkgs.hostPlatform.isDarwin then ".frontend" else ".webkit2gtk")
          else
            ""
        }";
    in project {
      inherit src compiler-nix-name plan-sha256 checkMaterialization;

      materialized = if (plan-sha256 != null) then
        (if js then
        # NOTE: https://github.com/input-output-hk/haskell.nix/issues/614
        # ./project.js.materialized
          null
        else if frontend then
          ./project.frontend.materialized
        else
          ./project.materialized)
      else
        null;

      cabalProject = readFile "${baseSrc}/${cabalProjectFile}";
      # NOTE https://github.com/input-output-hk/haskell.nix/issues/979#issuecomment-748483501
      # NOTE https://github.com/input-output-hk/tools/blob/95f44de0fb1d2ee6408ea0e2ca27cfd6967c02af/arm-test/default.nix#L45-L72
      # NOTE https://github.com/haskell/cabal/issues/6770#issue-615196643
      # cabal-install = (pkgs.haskell-nix.hackage-package {
      #   inherit compiler-nix-name;
      #   name = "cabal-install";
      #   version = "3.2.0.0";
      #   # https://github.com/input-output-hk/haskell.nix/issues/720#issuecomment-745397468
      #   modules = [{
      #     reinstallableLibGhc = true;
      #   }];
      # }).components.exes.cabal;

      modules = [{
        # NOTE https://github.com/input-output-hk/haskell.nix/issues/720#issuecomment-745397468
        # packages.cabal-install.reinstallableLibGhc = true;
        packages.servant-with-beam = {
          dontStrip = false;
          configureFlags = [ "--ghc-option=-O${optimization}" ];
        };
        # NOTE https://github.com/input-output-hk/haskell.nix/pull/336#discussion_r501772226
        packages.ekg.enableSeparateDataOutput = true;

        # NOTE https://github.com/wedens/yesod-cross-test-pg/blob/a9c46de9f0068686c8c256bc200e928d1de1c2d2/nix/default.nix#L17
        # NOTE https://github.com/input-output-hk/haskell.nix/pull/1056 NOTE seems not helpful
        packages."postgresql-libpq".patches =
          optional pkgs.hostPlatform.isWindows [
            (pkgs.runCommand "libpq_paths.patch" { } ''
              substitute ${
                ./nix/libpq_paths.patch
              } $out --subst-var-by libpq ${pkgs.libpq.out}
            '')
          ];
      }]
        # NOTE https://github.com/input-output-hk/haskell.nix/issues/86#issuecomment-472748457
        # NOTE https://github.com/entropia/tip-toi-reveng/blob/2a30c2500b804b31ed4536a186d3f123e18651ae/default.nix#L41
        ++ optional pkgs.hostPlatform.isMusl {
          packages.servant-with-beam.configureFlags =
            [ "--ghc-option=-static" ];
          # terminfo is disabled on musl by haskell.nix, but still the flag
          # is set in the package plan, so override this
          packages.haskeline.flags.terminfo = false;
        };

      index-state = "2021-03-19T00:00:00Z";
    };
  def = mkProject pkgs "0000000000000000000000000000000000000000000000000000";
  rp = (reflexPlatform {
    config.android_sdk.accept_license = true;
    nixpkgsFunc = obNixpkgsFunc ;
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
    };
    ios.servant-with-beam-frontend = {
      executableName = "frontend";
      bundleIdentifier = "my.frontend";
      bundleName = "IOS App";
    };
  });
  releases = {
    linux = def;
    osx = def;
    windows =
      mkProject mingwW64 "0000000000000000000000000000000000000000000000000000";
    static = mkProject static-pkgs
      "0000000000000000000000000000000000000000000000000000";
    # android = mkProject aarch64-android-prebuilt
    #   "0000000000000000000000000000000000000000000000000000";
    # ios =
    #   mkProject iphone64 "0000000000000000000000000000000000000000000000000000";
  };
  exes = mapAttrs (name: value:
    value.servant-with-beam.components.exes."${if frontend then
      "frontend"
    else
      "app"}") releases;
  shells = mapAttrs (name: value: value.shellFor) releases;
in if (default) then
  exes.${platform}
else {
  servant-with-beam = exes // {
    android = rp.android.servant-with-beam-frontend;
    ios = rp.ios.servant-with-beam-frontend;
  };
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
  pkgSet = pkgs;
  inherit shells;
}
