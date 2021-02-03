{ pkgs ? import ./nix/pkgs.nix { } }:
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
in pkgs.haskell-nix.project {
  inherit src;
  # For `cabal.project` based projects specify the GHC version to use.
  compiler-nix-name = "ghc8102"; # Not used for `stack.yaml` based projects.
  # modules = [{
  #   # packages.servantWithBeam.components.app.depends = with pkgs; [];
  #   reinstallableLibGhc = true;
  # }];
  index-state = "2021-01-05T00:00:00Z";
}
