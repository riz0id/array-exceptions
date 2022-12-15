args: 

import (import ./nixpkgs.nix) {
  config.packageOverrides = pkgs: 
    pkgs.lib.composeManyExtensions (map (f: f args) [  
      (import exts/array-exceptions.nix)
      (import exts/example.nix)
    ]) pkgs pkgs;
}