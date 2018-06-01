let
  nixpkgs = import <nixpkgs> {};
  haskellPackages = nixpkgs.haskellPackages;
  openzwave = import ./openzwave+centralscene.nix { pkgs = nixpkgs; };
  hozw = haskellPackages.callPackage (nixpkgs.fetchFromGitHub {
    owner = "Steell";
    repo = "haskell-openzwave";
    rev = "1cf5f2da3ec4afc96b0ac3a9c466f3c8669ac65a";
    sha256 = "1frjmmsj489ib7gz0ig18b5z7gksqnl9k9y6xgr95rjql61j653y";
  }) { inherit openzwave; };
in

{
  callCabal2nix
    ? haskellPackages.callCabal2nix,
  haskell-openzwave
    ? hozw
}:

callCabal2nix "smarthome" ./. { inherit haskell-openzwave; }
 
