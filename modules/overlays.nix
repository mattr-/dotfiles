{ ... }:
{
  flake.modules.nixos.overlays = {
    nixpkgs.overlays = [
      (final: _prev: {
        plannotator = final.callPackage ../pkgs/plannotator { };
      })
    ];
  };
}
