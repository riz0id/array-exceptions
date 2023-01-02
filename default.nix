{ ghc ? "ghc924" }:

import nix/pkgs.nix { 
  inherit ghc;
}
