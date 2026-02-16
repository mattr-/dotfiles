{ lib, ... }:
{
  flake.modules.nixos.stateVersion = {
    system.stateVersion = lib.mkDefault "24.05";
  };

  flake.modules.homeManager.stateVersion = {
    home.stateVersion = lib.mkDefault "24.05";
  };
}
