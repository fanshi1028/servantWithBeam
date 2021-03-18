{
  pkgs = hackage:
    {
      packages = {};
      compiler = { version = "8.10.4"; nix-name = "ghc8104"; packages = {}; };
      };
  extras = hackage:
    { packages = { servant-with-beam = ./.plan.nix/servant-with-beam.nix; }; };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "servant-with-beam" = {
            flags = {
              "frontend" = lib.mkOverride 900 true;
              "ghcid" = lib.mkOverride 900 false;
              };
            };
          };
        })
    ];
  }