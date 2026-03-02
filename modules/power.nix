{ ... }:
{
  flake.modules.nixos.power = { config, lib, ... }: lib.mkIf config.dots.power.enable {
    powerManagement.enable = true;

    services.logind.settings.Login = {
      HandleLidSwitchExternalPower = "suspend";
      HandlePowerKey = "suspend";
      HandleLidSwitch = "suspend";
    };

    services.power-profiles-daemon.enable = true;

    services.upower = {
      enable = true;
      percentageLow = 20;
      percentageCritical = 10;
      percentageAction = 5;
      criticalPowerAction = "Hibernate";
    };
  };
}
