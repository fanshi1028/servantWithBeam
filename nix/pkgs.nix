{ sources ? import ./sources.nix, static ? false }:
let
  # https://github.com/input-output-hk/haskell.nix/issues/741
  haskellNix = import sources.haskell-nix { };
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  overlays = haskellNix.overlays ++ [
    (_: super: {
      niv = import sources.niv { };
      haskell-nix = super.haskell-nix // {
        toolPackageName = super.haskell-nix.toolPackageName // {
          gen-hie = "implicit-hie";
        };
        packageToolName = super.haskell-nix.packageToolName // {
          implicit-hie = "gen-hie";
        };
      };
    })
  ];
  nixpkgsArgs = haskellNix.nixpkgsArgs // { inherit overlays; };
  static-haskell-nix = (import "${sources.static-haskell-nix}/survey" {
    normalPkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;
    inherit overlays;
    compiler = "ghc865";
  }).pkgs;
in if static then static-haskell-nix else import nixpkgsSrc nixpkgsArgs
