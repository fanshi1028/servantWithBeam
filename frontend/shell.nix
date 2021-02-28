{ compiler ? "ghc865", frontend ? true, useWarp ? true }:
import ../shell.nix { inherit compiler frontend useWarp; }
