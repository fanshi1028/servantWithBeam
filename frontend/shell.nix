{ checkMaterialization ? false }:
import ../shell.nix {
  inherit checkMaterialization;
  frontend = true;
  sha256 = "0bdgxp2f0gcjy1n0fqgkbz9qgjlbzw76f4a9pknlzyxwgvcgafhb";
}
