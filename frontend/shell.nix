{ checkMaterialization ? false }:
import ../shell.nix {
  inherit checkMaterialization;
  frontend = true;
  sha256 = "1zxx4gs0v8q6pa43a3n34xdnbqspndb6i9g1dhb6g5qdkvaljskp";
}
