{ ghc }:

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions [  
      (import exts/array-exceptions.nix {
        inherit ghc;
      })
    ] pkgs pkgs;
}