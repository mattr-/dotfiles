{
  lib,
  stdenv,
  fetchurl,
  autoPatchelfHook,
  makeWrapper,
  git,
  xdg-utils,
}:

stdenv.mkDerivation rec {
  pname = "plannotator";
  version = "0.8.1";

  src = fetchurl {
    url = "https://github.com/backnotprop/plannotator/releases/download/v${version}/plannotator-linux-x64";
    hash = "sha256-4R8a/ymSJo33dXMBatf63hGZQs8E72fAuyJEWRHAtLo=";
  };

  dontUnpack = true;

  nativeBuildInputs = [
    autoPatchelfHook
    makeWrapper
  ];

  buildInputs = [
    stdenv.cc.cc.lib
  ];

  installPhase = ''
    runHook preInstall

    install -Dm755 $src $out/bin/plannotator

    wrapProgram $out/bin/plannotator \
      --prefix PATH : ${
        lib.makeBinPath [
          git
          xdg-utils
        ]
      }

    runHook postInstall
  '';

  meta = {
    description = "Interactive plan review for AI coding agents";
    homepage = "https://plannotator.ai";
    license = with lib.licenses; [
      mit
      asl20
    ];
    platforms = [ "x86_64-linux" ];
    mainProgram = "plannotator";
  };
}
