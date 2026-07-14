{ ... }:
{
  flake.modules.nixos.sunshine = { config, lib, ...}: {
    config = lib.mkIf config.gui.enable {
      services.sunshine = {
        enable = true;
        autoStart = true;
        capSysAdmin = true;
        openFirewall = false;
      };
    };
  };
}
