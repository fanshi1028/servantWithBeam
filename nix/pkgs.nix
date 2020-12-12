# { sources ? import ./nix/sources.nix }: # import the sources
# with {
#   overlay = _: pkgs: {
#     niv = import sources.niv { }; # use the sources :)
#   };
# };
# import sources.nixpkgs # and use them again!
# {
#   overlays = [ overlay ];
#   config = { };
# }

{ # Fetch the latest haskell.nix and import its default.nix
# haskellNix ? (builtins.fetchTarball
#   "https://github.com/input-output-hk/haskell.nix/archive/master.tar.gz") { }

# (import ./nix/pkgs.nix { }).haskell-nix

# haskell.nix provides access to the nixpkgs pins which are used by our CI,
# hence you will be more likely to get cache hits when using these.
# But you can also just use your own, e.g. '<nixpkgs>'.
# , nixpkgsSrc ? haskellNix.sources.nixpkgs-2009

# haskell.nix provides some arguments to be passed to nixpkgs, including some
# patches and also the haskell.nix functionality itself as an overlay.
# , nixpkgsArgs ? haskellNix.nixpkgsArgs

# import nixpkgs with overlays
# , pkgs ? import nixpkgsSrc nixpkgsArgs
# import the sources

sources ? import ./sources.nix }: # import the sources
let
  # https://github.com/input-output-hk/haskell.nix/issues/741
  # Ways to override package
  haskellNix = import sources.haskell-nix {
    # sourcesOverride = {
    #   beam-core = import sources.beam { } + "/beam-core";
    #   # haskellNix.callCabal2nix "beam-core" (sources.beam + "/beam-core") { };
    #   # beam-sqlite = import sources.beam { } + "/beam-sqlite";
    #   # beam-migrate = import sources.beam { } + "/beam-migrate";
    # };
  };
  nixpkgsSrc = haskellNix.sources.nixpkgs-2009;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
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
      # beam-migrate = import sources.beam { } + "/beam-migrate";
      # haskellPackages = super.haskellPackages.override {
      #   overrides = hself: hsuper: {
      #     # beam-core =
      #     #   hsuper.callCabal2nix "beam-core" (sources.beam + "/beam-core") { };
      #     # beam-sqlite =
      #     #   hsuper.callCabal2nix "beam-sqlite" (sources.beam + "/beam-sqlite")
      #     #   { };
      #     # beam-migrate =
      #     #   hsuper.callCabal2nix "beam-migrate" (sources.beam + "/beam-migrate")
      #     #   { };
      #   };
      # };
    })
  ];
in import nixpkgsSrc (nixpkgsArgs // { inherit overlays; })
