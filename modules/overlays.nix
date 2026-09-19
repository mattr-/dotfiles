{ inputs, ... }:
let
  localOverlay = final: prev: {
    plannotator = final.callPackage ../pkgs/plannotator { };

    xwayland-satellite =
      let
        version = "0.8.2-unstable-2026-09-09";
        src = final.fetchFromGitHub {
          owner = "Supreeeme";
          repo = "xwayland-satellite";
          rev = "add2795134593faafce60e404a0a75df68e9ee0c";
          hash = "sha256-0TxfMgqW0/BLD4M942c5DCKYrtPvzsPJwvdcco4LQUM=";
        };
      in
      prev.xwayland-satellite.overrideAttrs (_oldAttrs: {
        inherit version src;

        cargoDeps = final.rustPlatform.fetchCargoVendor {
          pname = "xwayland-satellite";
          inherit version src;
          hash = "sha256-s1gl9eR6Mt2QLrhfcowstPFjzwE/lz4PJhJzWYHoIHg=";
        };
      });
  };

  overlays = [
    inputs.emacs-overlay.overlays.emacs
    localOverlay
  ];
in
{
  flake.overlays.default =
    inputs.nixpkgs.lib.composeManyExtensions overlays;

  flake.modules.nixos.overlays = {
    nixpkgs.overlays = overlays;
  };
}
