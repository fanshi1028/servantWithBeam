# FIXME Can't get it work, might try again in the future when flakes are more stable
{
  description = "Servant with beam";

  # inputs.nixpkgs.url = "nixpkgs";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  # inputs.nixpkgs.url = "github:input-output-hk/haskell.nix";

  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, haskellNix,
    # nixpkgs,
    flake-utils }:

    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        nixpkgs = haskellNix.legacyPackages.${system};
        project-args = {
          src = nixpkgs.nix-gitignore.gitignoreSource [ ] ./.;
          # src = nixpkgs.haskell-nix.haskellLib.cleanGit {
          #   name = "servant-with-beam";
          #   src  = ./.;
          # };
          compiler-nix-name = "ghc8102";
        };
        # overlay = self: super: {
        #   haskell-nix = super.haskell-nix // {
        #     toolPackageName = super.haskell-nix.toolPackageName // {
        #       gen-hie = "implicit-hie";
        #     };
        #     packageToolName = super.haskell-nix.packageToolName // {
        #       implicit-hie = "gen-hie";
        #     };
        #   };
        #   servant-with-beam = self.haskell-nix.project project-args;
        # };
        drv = nixpkgs.haskell-nix.project project-args;
      in {
        defaultPackage = drv.servant-with-beam;
      }
        # flake-utils.lib.simpleFlake {
      #   # defaultPackage =  project-args;

      #   name = "servant-with-beam";

      #   inherit self nixpkgs overlay;

      #   # shell = ./shell.nix;

      #   # systems = [ "x86_64-linux" "x86_64-darwin" ];
      #   systems = [ "x86_64-darwin" ];
      # }
    );
}
