let
  nixpkgs = import ./nix/nixpkgs.nix;
  pkgs = import nixpkgs {}; # replace with reference to pinned?
                            # find a better way to share this
  drv = import ./base-drv.nix; #eventually this would be based of a develop.nix
in drv.shellFor {
  packages = p: with p; [
#    haskell-openzwave
#    haskell-openzwave-gen
#    haskell-openzwave-cpp
    smarthome
  ];
  withHoogle = true;
  buildInputs = with pkgs; [
    cabal-install
    haskellPackages.ghcid
  ];
}

  # .env
  # .overrideAttrs (drv: {
  #   nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [
  #      # pkgs.haskell.packages.ghc844.ghcid
  #   ];
  # })
