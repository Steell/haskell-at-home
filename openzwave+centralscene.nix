{
  openzwave, fetchFromGitHub
}:

openzwave.overrideAttrs (oldAttrs: {
  src=fetchFromGitHub {
    owner = "OpenZWave";
    repo = "open-zwave";
    rev = "52a8451ad4676b157d081a77182c8496e4d20e99";
    sha256 = "145ywkzg4yj8rm27bi8n5y6b8jy7k7cvlicj4p3ha0v01v0nai2r";
  };
})
