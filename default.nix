{ compiler ? "ghc8102", static ? false
, pkgs ? import ./nix/pkgs.nix { inherit static compiler; }
, checkMaterialization ? false }:

let
  hLib = pkgs.haskell-nix.haskellLib;

  includedFiles = [ "app" "src" "tests" ];

  name = "servant-with-beam";
  baseSrc = ./.;
  # 'cleanGit' cleans a source directory based on the files known by git
  # NOTE https://github.com/input-output-hk/haskell.nix/issues/492
  # NOTE cleanGit not lorri friendly
  # NOTE use less good nix-gitignore instead for now
  # src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;
  # NOTE https://github.com/input-output-hk/haskell.nix/issues/1013
  src = hLib.cleanSourceWith {
    inherit name;
    filter = path: type:
      pkgs.lib.any (f:
        let p = toString (baseSrc + ("/" + f));
        in p == path || (pkgs.lib.strings.hasPrefix (p + "/") path))
      includedFiles || baseNameOf path == "${name}.cabal";
    src = baseSrc;
  };
  # For `cabal.project` based projects specify the GHC version to use.
  # compiler-nix-name = "ghc8102"; # Not used for `stack.yaml` based projects.
  compiler-nix-name = compiler; # Not used for `stack.yaml` based projects.

  # NOTE https://www.joachim-breitner.de/blog/776-Distributing_Haskell_programs_in_a_multi-platform_zip_file
  # NOTE https://github.com/entropia/tip-toi-reveng/blob/master/default.nix
  make-exe = pkgs: sha256:
    pkgs.haskell-nix.project {
      inherit src compiler-nix-name;
      index-state = "2021-01-05T00:00:00Z";
      # NOTE give up materializtion stuff for now
      # plan-sha256 = sha256;
      # inherit checkMaterialization;
      modules = [{
        packages.servant-with-beam.dontStrip = false;
        # NOTE https://github.com/input-output-hk/haskell.nix/pull/336#discussion_r501772226
        packages.ekg.enableSeparateDataOutput = true;
      }] ++ pkgs.lib.optional pkgs.hostPlatform.isMusl {
        packages.servant-with-beam.configureFlags = [

          # "--ghc-option=static"
          # "--ghc-option=-optl=-pthread"
          "--ghc-option=-optl=-L${
            pkgs.gmp6.override { withStatic = true; }
          }/lib"
          # "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
          # "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
        ];
        packages.haskeline.flags.terminfo = false;
      };
    } // {
      inherit (pkgs) heroku postgresql;
    };
in if static then
  make-exe pkgs.pkgsCross.musl64 ""
else
  make-exe pkgs "ZUA8Ft9cJaTXD1/wYw0KprGbQvLly4Zn7s1xJF0LPvY="
