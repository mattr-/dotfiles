{
  stdenv,
  lib,
  fetchFromGitHub,
  unzip,
}:
stdenv.mkDerivation rec {
  pname = "SF-Mono";
  version = "v1.0.0";

  src = fetchFromGitHub {
    owner = "mattr-";
    repo = "apple-sf-mono-fonts";
    rev = "main";
    sha256 = "sha256-qXQQ4R68/4AOmUxavN0sOcUsINiwkjTR5DEngNyLLWI=";
  };

  nativeBuildInputs = [unzip];

  installPhase = ''
    mkdir -p $out/share/fonts/opentype
    cp *.otf $out/share/fonts/opentype/
  '';

  meta = with lib; {
    description = "SF Mono Fonts";
    homepage = "https://github.com/mattr-/apple-sf-mono-fonts";
    license = licenses.mit;
    maintainers = [maintainers.mattr-];
  };
}
