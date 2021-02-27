{ compiler ? "ghc884", frontend ? true }:
import ../shell.nix { inherit compiler frontend; }
