{ lib, ... }:
{
  boot.loader = {
    systemd-boot = {
      enable = true;
      consoleMode = lib.mkDefault "max";
      configurationLimit = 5;
    };
    efi.canTouchEfiVariables = true;
  };
}
