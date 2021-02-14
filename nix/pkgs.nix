{ compiler ? "ghc8102" # unused here
, sources ? import ./sources.nix }:
let
  haskellNix = import sources.haskell-nix { };
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  overlays = haskellNix.overlays ++ [
    (_: super: {
      haskell-nix = super.haskell-nix // {
        toolPackageName = super.haskell-nix.toolPackageName // {
          gen-hie = "implicit-hie";
          haskell-language-server-wrapper = "haskell-language-server";
        };
        packageToolName = super.haskell-nix.packageToolName // {
          implicit-hie = "gen-hie";
          haskell-language-server = "haskell-language-server-wrapper";
        };
      };
    })
  ];
in import nixpkgsSrc (nixpkgsArgs // { inherit overlays; })
