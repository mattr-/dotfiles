{ ... }:
{
  flake.modules.nixos.nixpkgs = {
    nixpkgs.config.allowUnfree = true;
  };

  flake.modules.darwin.nixpkgs = {
    nixpkgs.config.allowUnfree = true;
  };
}
