{ sources ? import ./sources.nix, compiler ? "ghc865", static ? false }:
let
  haskellNix = if static then
    import sources.haskell-nix {
      pkgs = (import "${sources.static-haskell-nix}/survey" {
        inherit compiler;
      }).pkgs;
    }
  else
    import sources.haskell-nix { };
  # https://github.com/input-output-hk/haskell.nix/issues/741
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
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
  nixpkgsArgs = haskellNix.nixpkgsArgs // { inherit overlays; };
  # static-haskell-nix = (import "${sources.static-haskell-nix}/survey" {
  #   normalPkgs = import nixpkgsSrc haskellNix.nixpkgsArgs;
  #   inherit overlays compiler;
  # }).pkgs;
  # static-haskell-nix = (import "${sources.static-haskell-nix}/survey" {
  #   normalPkgs = import nixpkgsSrc nixpkgsArgs;
  #   inherit compiler;
  # }).pkgs;
  # https://input-output-hk.github.io/haskell.nix/tutorials/cross-compilation/#static-executables-with-musl-libc
in import nixpkgsSrc nixpkgsArgs
