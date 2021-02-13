{ compiler ? "ghc8103", pkgs ? import ./nix/pkgs.nix { inherit compiler; }
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
in pkgs.haskell-nix.project {
  inherit src;
  compiler-nix-name = compiler;
  modules = [{
    # NOTE https://github.com/input-output-hk/haskell.nix/issues/720#issuecomment-745397468
    reinstallableLibGhc = true;
    packages.servant-with-beam.dontStrip = false;
    # NOTE https://github.com/input-output-hk/haskell.nix/pull/336#discussion_r501772226
    packages.ekg.enableSeparateDataOutput = true;
  }];
  index-state = "2021-01-05T00:00:00Z";
} // {
  inherit (pkgs) heroku postgresql;
}
