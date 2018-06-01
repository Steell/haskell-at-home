let
  nixpkgs = import <nixpkgs> {};
  haskellPackages = nixpkgs.haskellPackages;
  openzwave = import ./openzwave+centralscene.nix { pkgs = nixpkgs; };
  hozw = haskellPackages.callPackage (nixpkgs.fetchFromGitHub {
    owner = "Steell";
    repo = "haskell-openzwave";
    rev = "ded32770eab9173d37632b5ce3fcd62b4ec0d641";
    sha256 = "0pyx4hrlrrzi8jj6vfc8k2lmvbq7nwkc5x31f4j3gpcfwh53iq8i";
  }) { inherit openzwave; };
in

{
  callCabal2nix
    ? haskellPackages.callCabal2nix,
  haskell-openzwave
    ? hozw
}:

callCabal2nix "smarthome" ./. { inherit haskell-openzwave; }
