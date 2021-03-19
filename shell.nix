{ js ? false, optimization ? "0", frontend ? js
, compiler ? if frontend then "ghc865" else "ghc8104", platform ? "osx"
, default ? false, checkMaterialization ? false, useWarp ? frontend && !js
, sha256 ? "", pkgs ? import ./default.nix {
  inherit platform compiler default checkMaterialization optimization js
    frontend useWarp sha256;
} }:
with pkgs;
shells.${platform} {
  # Include only the *local* packages of your project.
  packages = hs: with hs; [ servant-with-beam ];

  # Builds a Hoogle documentation index of all dependencies,
  # and provides a "hoogle" command to search the index.
  # Default is true anyway
  # withHoogle = true;

  # You might want some extra tools in the shell (optional).

  # Some common tools can be added with the `tools` argument
  tools = let
    mkArgs = plan-sha256: {
      inherit plan-sha256 checkMaterialization;
      compiler-nix-name = "ghc8104";
      version = "latest";
      index-state = "2021-03-19T00:00:00Z";
    };
  in {
    cabal = mkArgs "09jkfsjwlz1cwm7ibvjnisv61y3b422c16m68gsi4rr79klz67y9";
    hlint = mkArgs "0vl2pzaa1n3zl6dsghbmnb8ds6rw90h6wkm93m69rpz5gh5z2xab";
    haskell-language-server = let
      sha256 = if frontend then
        "0dxbn0h4lxjns444zf03mcqkj4qal3163nsc8976dy9xjl9sy9hn"
      else
        "1v0dqc5km7b7lryx2r0y98dd6n9afv1zlhzblvvjyzv4n5kghy47";
    in mkArgs sha256 // { compiler-nix-name = compiler; };
    ormolu = mkArgs "037lwr7kvnsi69dy4gh0x9gydrb01vx8da6ms7mfmq4zl9ah9ip1";
    ghcid = mkArgs "0nwqzk94rvi6dkspk1l1l1lyl44sm6d767h2i3nsi6bg8rm8dhi7";
    implicit-hie =
      mkArgs "1g592ckyzz61w99swhqb2zmkxwrcsg6c8jhgv9mmvas5c17dkqi7";
    retrie = mkArgs "10c7scmyynplrb816x36z7pffyyzk8vb395hsqphzy6n2ni4v3xp";
    doctest = mkArgs "0gr73r802d6w2f842ap5a0hvwria7gnr5aqfgxrrhmd4qghil37q";
    cabal-fmt = mkArgs "1xliws2pxpw5z4pizfn6nhrd70bhgkqv2wvwvlskk9y6a533g3dw"
      // {
        cabalProject = ''
          packages: .
                 '';
      };
  };
  # See overlays/tools.nix for more details

  # Some you may need to get some other way.
  buildInputs = with pkgSet;
    (if frontend then [ ] else [ postgresql heroku ])
    ++ [ niv nodePackages.prettier ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
