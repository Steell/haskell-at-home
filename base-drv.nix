let

  config = {
    allowUnfree = true;
    allowBroken = true; # otherwise hoppy won't build, even though we're overlaying it
  };

  overlays = [
    (self: super: let
      hoppy-pin = super.lib.importJSON ./hoppy-pin.json;
      hoppy-repo = fetchGit {
        name = "hoppy-2020-05-29";
        inherit (hoppy-pin) url rev;
      };
      hoppy-overlay = import "${hoppy-repo}/nix/overlay.nix";
    in hoppy-overlay self super)
  ];

  nixpkgs = import ./nix/nixpkgs.nix;
  pkgs = import nixpkgs { inherit config overlays; };

in (pkgs.haskellPackages.override {
  overrides = hNew: hOld: with pkgs.haskell.lib; rec {
    # Get it to build
    horizon = doJailbreak hOld.horizon;
    reactive-banana = doJailbreak hOld.reactive-banana;

    # Actual stuff
    haskell-openzwave = hNew.callPackage ./haskell-openzwave/haskell-openzwave {};
    haskell-openzwave-cpp = hNew.callPackage ./haskell-openzwave/haskell-openzwave-cpp {
      # TODO: would be great if we didn't have to do this manually
      inherit overrideCabal;
    };
    haskell-openzwave-gen = hNew.callPackage ./haskell-openzwave/haskell-openzwave-gen {};
    smarthome = hNew.callPackage ./. {};
  };
})
