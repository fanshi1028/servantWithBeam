{ checkMaterialization ? false }:
import ../shell.nix {
  inherit checkMaterialization;
  frontend = true;
  sha256 = "03csdlik5jmk2hy0y9bsv8sygl5ys2bp5v8c454zkwq6x87n8mff";
}
