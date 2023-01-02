{ ghc ? "ghc942" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.mkShell {
  buildInputs = [
    (pkgs.haskell.packages."${ghc}".haskell-language-server)
  ];
}