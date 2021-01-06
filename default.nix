{ pkgs ? import ./nix/pkgs.nix { } }:
pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  # NOTE https://github.com/input-output-hk/haskell.nix/issues/492
  # NOTE cleanGit not lorri friendly
  # src = pkgs.haskell-nix.haskellLib.cleanGit {
  #   name = "servantWithBeam";
  #   src = ./.;
  # };
  # NOTE use less good nix-gitignore instead for now
  src = pkgs.nix-gitignore.gitignoreSource [ ] ./.;

  # For `cabal.project` based projects specify the GHC version to use.
  compiler-nix-name = "ghc8102"; # Not used for `stack.yaml` based projects.
  # modules = [{
  #   # packages.servantWithBeam.components.app.depends = with pkgs; [
  #   #   beam-core
  #   # ];
  #   reinstallableLibGhc = true;
  # }];
  index-state = "2021-01-05T00:00:00Z";
}
