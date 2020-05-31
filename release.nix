let

  config = {
    allowUnfree = true;
    allowBroken = true;
    # packageOverrides = pkgs: rec {
      # haskellPackages = pkgs.haskell.packages.ghc84;
    # };
  };

  overlays = [

    (self: super: let
      hoppy-pin = super.lib.importJSON ./hoppy-pin.json;
      hoppy-repo = fetchGit { #super.fetchFromGitLab {
        name = "hoppy-2020-05-29";
        inherit (hoppy-pin) url rev;
      };
      hoppy-overlay = import "${hoppy-repo}/nix/overlay.nix";
    in hoppy-overlay self super)

    (newPkgs: oldPkgs: with oldPkgs.haskell.lib; {

      openzwave = newPkgs.callPackage ./openzwave+centralscene.nix {
        inherit (oldPkgs) openzwave;
      };

    })
  ];

  nixpkgs = import ./nix/nixpkgs.nix;

  pkgs = import nixpkgs { inherit config overlays; };

in (pkgs.haskellPackages.override {
  overrides = hNew: hOld:
  # let
  #   fixup = p: dontHaddock (dontCheck p);
  # in
  with pkgs.haskell.lib;
  rec {
    # Get it to build
    horizon = doJailbreak hOld.horizon;
    reactive-banana = doJailbreak hOld.reactive-banana;

    # Actual stuff
    haskell-openzwave = hNew.callPackage ./haskell-openzwave/haskell-openzwave {};
    haskell-openzwave-cpp = hNew.callPackage ./haskell-openzwave/haskell-openzwave-cpp {
      inherit overrideCabal;
    };
    haskell-openzwave-gen = hNew.callPackage ./haskell-openzwave/haskell-openzwave-gen {};
    smarthome = hNew.callPackage ./. {};
  };
})
# .extend (pkgs.haskell.lib.packageSourceOverrides {

# });
# { inherit (pkgs.haskellPackages) haskell-openzwave smarthome; }
