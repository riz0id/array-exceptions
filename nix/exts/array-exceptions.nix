{ ghc }:

final: prev: 

{
  haskell = prev.haskell // {
    packages = prev.haskell.packages // {
      "${ghc}" = prev.haskell.packages."${ghc}".extend (self: _: {
        array-exceptions = self.callCabal2nix "array-exceptions" ../../. { };
      });
    };
  };
}