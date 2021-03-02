{ js ? false, optimization ? "0", frontend ? js
, compiler ? if frontend then "ghc865" else "ghc8104", platform ? "osx"
, default ? false, checkMaterialization ? false, useWarp ? frontend && !js
, pkgs ? import ./default.nix {
  inherit platform compiler default checkMaterialization optimization js
    frontend useWarp;
} }:
with pkgs;
shells.${platform} {
  # shellFor {
  # Include only the *local* packages of your project.
  packages = hs: with hs; [ servant-with-beam ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  # Default is true anyway
  # withHoogle = true;

  # You might want some extra tools in the shell (optional).

  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
    ormolu = "latest";
    ghcid = "latest";
    implicit-hie = "latest";
    retrie = "latest";
    doctest = "latest";
    cabal-fmt = {
      compiler-nix-name =
        "ghc8104"; # seems different cabal version come with ghc fuck this up and I don't know how to override that cabal
      version = "latest";
      cabalProject = ''
        packages: .
               '';
    };
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = (if frontend then [ ] else [ postgresql heroku ]) ++ [ niv ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
