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
  static-pkgs = (import "${sources.static-haskell-nix}/survey" {
    inherit normalPkgs compiler overlays;
  }).pkgs;
  # in import nixpkgsSrc (nixpkgsArgs // { inherit overlays; })
  #   args = nixpkgsArgs // { inherit overlays; };
  # in {
  #   pkgs = import nixpkgsSrc args;
  #   osx-pkgs = import nixpkgsSrc (args // { system = "x86_64-darwin"; });
  # }
  pkgs = import nixpkgsSrc (nixpkgsArgs // { inherit overlays; });

  # NOTE https://github.com/wedens/yesod-cross-test-pg/blob/a9c46de9f0068686c8c256bc200e928d1de1c2d2/nix/release.nix#L5
  windowOverlays = [
    (self: super: {
      libpq = super.callPackage ./postgresql-prebuild.nix {
        inherit (super.buildPackages) fetchurl unzip;
      };
    })
  ];

  win64-pkgs = import nixpkgsSrc
    (nixpkgsArgs // { overlays = overlays ++ windowOverlays; });
in { inherit pkgs static-pkgs win64-pkgs; }
