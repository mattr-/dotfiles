{ inputs, ... }:
{
  flake.modules.nixos.overlays = {
    nixpkgs.overlays = [
      inputs.emacs-overlay.overlays.emacs

      (final: _prev: {
        plannotator = final.callPackage ../pkgs/plannotator { };
      })
    ];
  };
}
