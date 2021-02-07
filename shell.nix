{ pkgs ? import ./default.nix { } }:
pkgs.shellFor {
  # Include only the *local* packages of your project.
  packages = ps: with ps; [ servant-with-beam ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  withHoogle = true;

  # You might want some extra tools in the shell (optional).

  # Some common tools can be added with the `tools` argument
  tools = {
    cabal = "3.2.0.0";
    hlint = "3.2.2";
    haskell-language-server = "0.8.0";
    ormolu = "0.1.4.1";
    ghcid = "0.8.7";
    implicit-hie = "0.1.2.3";
    retrie = "0.1.1.1";
    doctest = "0.17";
    cabal-bounds = "2.3.0";
    cabal-fmt = {
      version = "0.1.5";
      cabalProject = ''
        packages: .
               '';
    };
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = with pkgs; [
    postgresql
    heroku
  ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
