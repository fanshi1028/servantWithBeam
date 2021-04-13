{ checkMaterialization ? false }:
import ../shell.nix {
  inherit checkMaterialization;
  frontend = true;
  sha256 = "0fi60bszfb8d59v1k18isakry607x6v6qdaf3nyxahx34hyv8c0c";
}
