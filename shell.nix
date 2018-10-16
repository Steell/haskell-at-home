let
  pkgs = import <nixpkgs> {}; # replace with reference to pinned?
                              # find a better way to share this

  # https://github.com/haskell/haskell-ide-engine#using-vs-code-with-nix
  hie = (import (pkgs.fetchFromGitHub {
      owner="domenkozar";
      repo="hie-nix";
      rev="96af698f0cfefdb4c3375fc199374856b88978dc";
      sha256="1ar0h12ysh9wnkgnvhz891lvis6x9s8w3shaakfdkamxvji868qa";
    }) {})
    .hie84;
  vscode = pkgs.vscode.overrideDerivation (old: {
      postFixup = old.postFixup + ''
        wrapProgram $out/bin/code --prefix PATH : ${pkgs.lib.makeBinPath [hie]}
      '';
    });

in

(import ./release.nix)
  .smarthome
  .env
  .overrideAttrs (drv: {
    buildInputs = (drv.buildInputs or []) ++ [
        pkgs.cabal-install
        hie
        vscode
      ];
  })
