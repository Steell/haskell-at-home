{
  callCabal2nix, makeWrapper, openzwave
}:

let
  pkg = callCabal2nix "smarthome" ./. {};
in
pkg.overrideAttrs (oldAttrs: {
  buildInputs = oldAttrs.buildInputs or [] ++ [ makeWrapper ];
  postInstall = oldAttrs.postInstall or "" + ''
    wrapProgram $out/bin/smarthome-server \
      --add-flags "${openzwave}/etc/openzwave"
  '';
})
