{ compiler ? "ghc8104", platform ? "osx", optimization ? "0", default ? false
, pkgs ? import ./default.nix {
  inherit platform compiler default checkMaterialization optimization frontend;
}, frontend ? false, checkMaterialization ? false }:
with pkgs;
shells.${platform} {
  # shellFor {
  # Include only the *local* packages of your project.
  packages = hs: with hs; [ servant-with-beam ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # You might want some extra tools in the shell (optional).

  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.2";
    haskell-language-server = "0.9.0.0";
    ormolu = "0.1.4.1";
    ghcid = "0.8.7";
    implicit-hie = "0.1.2.3";
    retrie = "0.1.1.1";
    doctest = "0.17";
    cabal-fmt = {
      compiler-nix-name =
        "ghc8104"; # seems different cabal version come with ghc fuck this up and I don't know how to override that cabal
      version = "0.1.5.1";
      cabalProject = ''
        packages: .
               '';
    };
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = [ postgresql heroku ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
