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
  # staticHaskellNix = import "${sources.static-haskell-nix}/survey" { inherit compiler overlays; };
  # normalPkgs = import nixpkgsSrc (nixpkgsArgs // { inherit overlays; });
  # staticHaskellNix = import "${sources.static-haskell-nix}/survey" { inherit normalPkgs compiler; };
  normalPkgs = import nixpkgsSrc nixpkgsArgs;
  static-pkgs = import "${sources.static-haskell-nix}/survey" {
    inherit normalPkgs compiler overlays;
  };
  # in import nixpkgsSrc (nixpkgsArgs // { inherit overlays; })
  #   args = nixpkgsArgs // { inherit overlays; };
  # in {
  #   pkgs = import nixpkgsSrc args;
  #   osx-pkgs = import nixpkgsSrc (args // { system = "x86_64-darwin"; });
  # }
  pkgs = import nixpkgsSrc (nixpkgsArgs // { inherit overlays; });
in { inherit pkgs static-pkgs; }
