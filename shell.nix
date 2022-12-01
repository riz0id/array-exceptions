{ ghc ? "ghc924" }:

let 
  pkgs = import ./default.nix { 
    inherit ghc; 
  };
in pkgs.array-exceptions.env.overrideAttrs (self: {
  buildInputs = self.buildInputs ++ (with pkgs; [ 

  ]);
})