{ ... }:
{
  flake.modules.nixos.sunshine = { config, lib, ... }: lib.mkIf config.dots.sunshine.enable {
    services.sunshine = {
      enable = true;
      autoStart = true;
      capSysAdmin = true;
      openFirewall = true;
    };
  };
}
