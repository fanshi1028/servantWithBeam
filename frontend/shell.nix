{ useWarp ? true }: (import ../default.nix { inherit useWarp; }).shells.ghc
