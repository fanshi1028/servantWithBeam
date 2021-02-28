{ compiler ? "ghc8102", sources ? import ./sources.nix }:
let
  haskellNix = import sources.haskell-nix { };
  # nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsSrc = haskellNix.sources.nixpkgs-unstable;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
  overlays = haskellNix.overlays ++ [
    (_: super: {
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

  pkgs = import nixpkgsSrc (nixpkgsArgs // { inherit overlays; });

  # NOTE https://github.com/NixOS/nixpkgs/issues/85924#issuecomment-640277067
  # NOTE https://github.com/NixOS/nixpkgs/issues/89769
  # static-pkgs = (import "${sources.static-haskell-nix}/survey" {
  #   inherit compiler;
  #   normalPkgs = pkgs;
  # }).pkgs;
  # NOTE try using nixpkgs pin from static-haskell-nix
  # static-pkgs = (import "${sources.static-haskell-nix}/survey" {
  #   inherit compiler;
  #   normalPkgs =
  #     (import "${sources.static-haskell-nix}/nixpkgs.nix").appendOverlays
  #     ([ (_: _: nixpkgsArgs) ] ++ overlays);
  # }).pkgs;
  static-pkgs = (import "${sources.static-haskell-nix}/survey" {
    inherit compiler;
    normalPkgs = import nixpkgsSrc nixpkgsArgs;
  }).pkgs.appendOverlays overlays;
  # staticHaskellNix = import "${sources.static-haskell-nix}/survey" { inherit compiler overlays; };
  # static-pkgs = (import "${sources.static-haskell-nix}/survey" {
  #   inherit normalPkgs compiler overlays;
  # }).pkgs;

  windowOverlays = [
    (self: super: {
      # NOTE https://github.com/wedens/yesod-cross-test-pg/blob/a9c46de9f0068686c8c256bc200e928d1de1c2d2/nix/release.nix#L5
      libpq = super.callPackage ./postgresql-prebuild.nix {
        inherit (super.buildPackages) fetchurl unzip;
      };
      icuin = self.icu;
      icudt = self.icu;
    })
  ];

  win64-pkgs = import nixpkgsSrc
    (nixpkgsArgs // { overlays = overlays ++ windowOverlays; });


in { inherit pkgs static-pkgs win64-pkgs; }
