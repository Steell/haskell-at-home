let

  config = {
    allowUnfree = true;
    packageOverrides = pkgs: rec {
      haskellPackages = pkgs.haskell.packages.ghc843;
    };
  };

  overlays = [
    (newPkgs: oldPkgs: with oldPkgs.haskell.lib; {

      haskellPackages = oldPkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackagesOld: {

          #unliftio = doJailbreak haskellPackagesOld.unliftio;
          #stm = doJailbreak haskellPackagesOld.stm;

          haskell-openzwave =
            haskellPackagesNew.callPackage ./haskell-openzwave { };

          smarthome =
            haskellPackagesNew.callPackage ./default.nix { };

        };
      };

    })
  ];

  nixpkgs = <nixpkgs>; #import ./nix/17_09.nix;

  pkgs = import nixpkgs { inherit config overlays; };

in
{ inherit (pkgs.haskellPackages) smarthome; }
