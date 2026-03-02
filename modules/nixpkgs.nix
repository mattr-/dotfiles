{ ... }:
let
  plannotatorOverlay = final: _prev: {
    plannotator = final.callPackage ../pkgs/plannotator { };
  };
in
{
  flake.overlays.default = plannotatorOverlay;

  flake.modules.nixos.nixpkgs = {
    nixpkgs.config.allowUnfree = true;
    nixpkgs.overlays = [ plannotatorOverlay ];
  };

  flake.modules.darwin.nixpkgs = {
    nixpkgs.config.allowUnfree = true;
  };
}
