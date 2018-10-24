{
  openzwave, fetchFromGitHub
}:

openzwave.overrideAttrs (oldAttrs: {
  src=fetchFromGitHub {
    owner = "OpenZWave";
    repo = "open-zwave";
    rev = "1099fa729e28c863c311837bf4da852b8d2a80e9";
    sha256 = "0wgc1wh0jppkmx585s83im4m7nv7q8fxnyia8cnjqkk8hxlp3yd8";
  };
})
