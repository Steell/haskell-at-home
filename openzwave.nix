{
  lib, openzwave, fetchFromGitHub
}:

openzwave.overrideAttrs (oldAttrs: {
  version = "1.6";
  src = fetchFromGitHub (lib.importJSON ./openzwave-pin.json);
})
