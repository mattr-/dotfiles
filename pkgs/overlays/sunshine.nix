# Bumps `sunshine` past nixpkgs' current 2026.516.143833 to upstream
# v2026.906.222525 (a security-fix release), since no nixpkgs PR targets it
# yet. Carries forward the tray-backend switch from libappindicator to Qt6
# that landed in nixpkgs PR #533497 (still open, only targets the older
# v2026.713.170739 pre-release), which upstream's tray library still needs
# at this version.
final: prev:
let
  buildDepsTag = "v2026.724.203728";
  ffmpegArch =
    {
      x86_64-linux = "Linux-x86_64";
      aarch64-linux = "Linux-aarch64";
      aarch64-darwin = "Darwin-arm64";
    }
    .${final.stdenv.hostPlatform.system}
      or (throw "sunshine overlay: unsupported system ${final.stdenv.hostPlatform.system}");
  ffmpegHash =
    {
      x86_64-linux = "sha256-ERw553AsQ0s/7oEXCiwjJjZEp1hpe9aCgiEBRs0K0R0=";
      aarch64-linux = "sha256-QR1PZZdWJbPEdRHb8oM3YWHGAV0+ujvqc0xg5n3M8H8=";
      aarch64-darwin = "sha256-dqZN31HL7wg6MTYPCsMA69wfx1pPHmMjHJa2ym0AXFM=";
    }
    .${final.stdenv.hostPlatform.system};
  ffmpegPrebuilt = final.fetchzip {
    url = "https://github.com/LizardByte/build-deps/releases/download/${buildDepsTag}/${ffmpegArch}-ffmpeg.tar.gz";
    hash = ffmpegHash;
  };
in
{
  sunshine = prev.sunshine.overrideAttrs (
    old:
    let
      inherit (final.stdenv.hostPlatform) isLinux;
    in
    rec {
      version = "2026.906.222525";

      src = final.fetchFromGitHub {
        owner = "LizardByte";
        repo = "Sunshine";
        tag = "v${version}";
        hash = "sha256-2Ab8KSX/SY3OFeqhwgYmMX/HQdV/jcCpNAFOEdG3V8w=";
        fetchSubmodules = true;
      };

      # buildNpmPackage bakes its internal npm-fetch derivation in at
      # construction time, so old.ui.overrideAttrs can't change npmDepsHash
      # (it'd just set an unused attribute on the already-built result) -
      # reconstruct it instead, mirroring nixpkgs' own `ui` definition.
      ui = final.buildNpmPackage {
        inherit src version;
        pname = "sunshine-ui";
        npmDepsHash = "sha256-w9/11m9PnwxuA9qJJH3JiGwperBSKNUGW2QAHFiejxo=";

        installPhase = ''
          runHook preInstall

          mkdir -p "$out"
          cp -a . "$out"/

          runHook postInstall
        '';
      };

      # replace the ffmpeg prebuilt fixed-output derivation baked into the
      # original cmakeFlags with one for our newer buildDepsTag
      cmakeFlags = (final.lib.filter (f: !(final.lib.hasPrefix "-DFFMPEG_PREPARED_BINARIES=" f)) old.cmakeFlags) ++ [
        (final.lib.cmakeFeature "FFMPEG_PREPARED_BINARIES" "${ffmpegPrebuilt}")
      ];

      nativeBuildInputs = old.nativeBuildInputs ++ final.lib.optionals isLinux [
        final.qt6.wrapQtAppsHook
      ];

      buildInputs = (final.lib.remove final.libappindicator old.buildInputs) ++ final.lib.optionals isLinux [
        final.qt6.qtbase
        final.qt6.qtsvg
      ];

      dontWrapQtApps = true;

      postFixup = final.lib.optionalString isLinux ''
        wrapProgram $out/bin/sunshine \
          "''${qtWrapperArgs[@]}" \
          ${final.lib.optionalString (final.config.cudaSupport or false) "--set LD_LIBRARY_PATH ${final.lib.makeLibraryPath [ final.vulkan-loader ]}"}
      '';
    }
  );
}
