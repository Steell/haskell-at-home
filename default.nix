let
  nixpkgs = import <nixpkgs> {};
  haskellPackages = nixpkgs.haskellPackages;  #haskell.packages.ghc822;
  # openzwave = import ./open-zwave { pkgs = nixpkgs; };
  hozw = haskellPackages.callPackage ./haskell-openzwave {
    #inherit openzwave;
  };
in

{
  callCabal2nix
    ? haskellPackages.callCabal2nix,
  haskell-openzwave
    ? hozw
}:

callCabal2nix "smarthome" ./. { inherit haskell-openzwave; }
