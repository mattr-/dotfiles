{ inputs, ... }:
let
  overlays = [
    inputs.emacs-overlay.overlays.emacs
  ];
in
{
  flake.overlays.default =
    inputs.nixpkgs.lib.composeManyExtensions overlays;

  flake.modules.nixos.overlays = {
    nixpkgs.overlays = overlays;
  };
}
