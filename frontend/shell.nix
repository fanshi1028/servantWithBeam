{ checkMaterialization ? false }:
import ../shell.nix {
  inherit checkMaterialization;
  frontend = true;
  sha256 = "05cn5463494qp9sgkxk80h6fqhwxi5xs9s80raxwbrgsd8900ixx";
}
