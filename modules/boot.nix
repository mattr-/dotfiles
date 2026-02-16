{ lib, ... }:
{
  flake.modules.nixos.boot = {
    boot.loader = {
      systemd-boot = {
        enable = lib.mkDefault true;
        consoleMode = lib.mkDefault "max";
        configurationLimit = 5;
      };
      efi.canTouchEfiVariables = lib.mkDefault true;
    };
  };
}
