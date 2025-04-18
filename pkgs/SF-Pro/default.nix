{
  stdenv,
  lib,
  fetchFromGitHub,
  unzip,
}:
stdenv.mkDerivation rec {
  pname = "SF-Pro";
  version = "v4.0.0";

  src = fetchFromGitHub {
    owner = "mattr-";
    repo = "apple-sf-pro-fonts";
    rev = "main";
    sha256 = "sha256-qdz1Y9YXZxy/PuB5ZrnGLexZQ+G1L5DJYFj9lwrU6M4=";
  };

  nativeBuildInputs = [unzip];

  installPhase = ''
    mkdir -p $out/share/fonts/opentype
    mkdir -p $out/share/fonts/truetype
    cp *.otf $out/share/fonts/opentype
    cp *.ttf $out/share/fonts/truetype
  '';

  meta = with lib; {
    description = "San Francisco Pro Fonts";
    homepage = "https://github.com/mattr-/apple-sf-pro-fonts";
    license = licenses.mit;
    maintainers = [maintainers.mattr-];
  };
}
