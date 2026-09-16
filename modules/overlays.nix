{ inputs, ... }:
let
  localOverlay = final: prev: {
    plannotator = final.callPackage ../pkgs/plannotator { };

    xwayland-satellite =
      let
        version = "0.8.1";
        src = final.fetchFromGitHub {
          owner = "Supreeeme";
          repo = "xwayland-satellite";
          tag = "v${version}";
          hash = "sha256-BUE41HjLIGPjq3U8VXPjf8asH8GaMI7FYdgrIHKFMXA=";
        };
      in
      prev.xwayland-satellite.overrideAttrs (_oldAttrs: {
        inherit version src;

        cargoDeps = final.rustPlatform.fetchCargoVendor {
          pname = "xwayland-satellite";
          inherit version src;
          hash = "sha256-16L6gsvze+m7XCJlOA1lsPNELE3D364ef2FTdkh0rVY=";
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
