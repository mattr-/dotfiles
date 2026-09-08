{ inputs, ... }:
let
  localOverlay = final: _prev: {
    plannotator = final.callPackage ../pkgs/plannotator { };
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
