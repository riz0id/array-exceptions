{ ghc ? "ghc924" }:

let
  pkgs = import nix/pkgs.nix { 
    inherit ghc;
  };
in {
  inherit (pkgs) 
    mkShell;

  inherit (pkgs.haskell.packages."${ghc}") 
    array-exceptions
    example
    hlint
    haskell-language-server;
}
