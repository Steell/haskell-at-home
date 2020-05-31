{
  openzwave, fetchFromGitHub
}:

openzwave.overrideAttrs (oldAttrs: {
  src = fetchFromGitHub {
    owner = "OpenZWave";
    repo = "open-zwave";
    rev = "2a76bdcd04513aa02618cc34432f8d5242fc73d1";
    sha256 = "1dl95nhz8dm9ng9sd2mmg7ja7zzgy4lzqgvcyqpkqg2rg6ghgagq";
  };
})
