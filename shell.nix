{ js ? false, optimization ? "0", frontend ? js
, compiler ? if frontend then "ghc865" else "ghc8104", platform ? "osx"
, default ? false, checkMaterialization ? false, useWarp ? frontend && !js
, sha256 ? "0lx3n9zhfss2n05wvfcn16fp6hw4fwvf3778yr5afzwh90i1njiz", pkgs ? import ./default.nix {
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
      index-state = "2021-03-02T00:00:00Z";
    };
  in {
    cabal = mkArgs "139rs4ir092qlb672nv5ad01bwm28n32nrybx9spqns835rq0110";
    hlint = mkArgs "1gdzw7b7vwygcd077am5lhj6l0s4qllvnqj6dpmb1bz4sapnsjf4";
    haskell-language-server = let
      sha256 = if frontend then
        "0hdr4wqkghx7cfhlp9p7d96wlcryk3740v41lb1gj6x9951rh68w"
      else
        "1d5avpz1rhdvhq4b6s8jc5kr4jcywpwy3hsfqydj3hhz7m8h5lcr";
    in mkArgs sha256 // { compiler-nix-name = compiler; };
    ormolu = mkArgs "037lwr7kvnsi69dy4gh0x9gydrb01vx8da6ms7mfmq4zl9ah9ip1";
    ghcid = mkArgs "0nwqzk94rvi6dkspk1l1l1lyl44sm6d767h2i3nsi6bg8rm8dhi7";
    implicit-hie =
      mkArgs "0i15qi2kj4qq2zczfagq2aibapn1v9yrf78s1f79ighfp55ahrcn";
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
  buildInputs = (if frontend then [ ] else [ postgresql heroku ]) ++ [ niv ];

  # Prevents cabal from choosing alternate plans, so that
  # *all* dependencies are provided by Nix.
  exactDeps = true;
}
