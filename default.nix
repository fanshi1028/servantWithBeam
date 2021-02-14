{ compiler ? "ghc8104", platform ? "osx", default ? true
, pkgs ? import ./nix/pkgs.nix { inherit compiler; }
, checkMaterialization ? false }:
let
  # inherit (pkgsSets) pkgs osx-pkgs;
  inherit (pkgs.pkgsCross) mingwW64 musl64;
  inherit (pkgs.lib.attrsets) mapAttrs;

  includedFiles = [ "app" "src" "tests" ];

  name = "servant-with-beam";
  baseSrc = ./.;

  compiler-nix-name = compiler;

  mkProject = pkgs: sha256:
    let
      inherit (pkgs.haskell-nix) haskellLib project;
      inherit (pkgs.lib) any strings optional attrsets;
      # 'cleanGit' cleans a source directory based on the files known by git
      # NOTE https://github.com/input-output-hk/haskell.nix/issues/492
      # NOTE cleanGit not lorri friendly
      # NOTE use less good nix-gitignore instead for now
      # src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
      # NOTE https://github.com/input-output-hk/haskell.nix/issues/1013
      filter = path: type:
        any (f:
          let p = toString (baseSrc + ("/" + f));
          in p == path || (strings.hasPrefix (p + "/") path)) includedFiles
        || baseNameOf path == "${name}.cabal";
      src = haskellLib.cleanSourceWith {
        inherit name filter;
        src = baseSrc;
      };
    in project {
      inherit src compiler-nix-name;
      # NOTE https://github.com/input-output-hk/haskell.nix/issues/979#issuecomment-748483501
      # NOTE https://github.com/input-output-hk/tools/blob/95f44de0fb1d2ee6408ea0e2ca27cfd6967c02af/arm-test/default.nix#L45-L72
      # cabal-install = (pkgs.haskell-nix.hackage-package {
      #   inherit compiler-nix-name;
      #   name = "cabal-install";
      #   version = "3.2.0.0";
      #   # https://github.com/input-output-hk/haskell.nix/issues/720#issuecomment-745397468
      #   modules = [{
      #     reinstallableLibGhc = true;
      #   }];
      # }).components.exes.cabal;
      #
      # plan-sha256 = sha256;
      modules = [{
        # NOTE https://github.com/input-output-hk/haskell.nix/issues/720#issuecomment-745397468
        # reinstallableLibGhc = true;
        # packages.Cabal.reinstallableLibGhc = true;
        packages.servant-with-beam.dontStrip = false;
        # NOTE https://github.com/input-output-hk/haskell.nix/pull/336#discussion_r501772226
        packages.ekg.enableSeparateDataOutput = true;
      }] ++ optional pkgs.hostPlatform.isMusl {
        packages.servant-with-beam.configureFlags = [ "--ghc-option=-static" ];
        # terminfo is disabled on musl by haskell.nix, but still the flag
        # is set in the package plan, so override this
        packages.haskeline.flags.terminfo = false;
      };
      index-state = "2021-02-13T23:31:09Z";
    };
  # app = pkgs: sha256:
  #   (mkProject pkgs sha256).servant-with-beam.components.exes.app;
  # osx-exe = app osx-pkgs "temp";
  # exe = app pkgs "temp";
  # releases = {
  #   servant-with-beam = exe;
  #   servant-with-beam-linux = exe;
  #   servant-with-beam-osx = exe;
  #   servant-with-beam-windows = app mingwW64 "temp";
  #   servant-with-beam-static = app musl64 "temp";
  # };
  def = mkProject pkgs "tmep";
  releases = {
    linux = def;
    osx = def;
    windows = mkProject mingwW64 "temp";
    static = mkProject musl64 "temp";
  };
  exes = mapAttrs (name: value: value.servant-with-beam.components.exes.app)
    releases;
  shells = mapAttrs (name: value: value.shellFor) releases;
in if (default) then
  exes.${platform}
else {
  servant-with-beam = exes;
  inherit (pkgs)
  # For docker release
    dockerTool busybox
    # For nix-shell
    heroku postgresql;
  inherit shells;
}
