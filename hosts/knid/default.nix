{inputs, outputs, pkgs, ...}: {
  imports = [
    inputs.hardware.nixosModules.framework-13-7040-amd
    ./hardware-configuration.nix
    ../_global/linux
    ../_global/linux/graphical/greetd.nix
    ../_global/linux/graphical/hyprland.nix
    ../_global/linux/steam.nix
    ../_global/linux/sysctl.nix
    ../_global/linux/systemd-boot.nix
    ../../users/mattr-/nixos.nix
  ];

  networking = {
    hostName = "knid";
  };

  programs.niri.enable = true;

  powerManagement = {
    enable = true;
  };

  services = {
    logind = {
      powerKey = "suspend";
      lidSwitch = "suspend";
      lidSwitchExternalPower = "lock";
    };

    power-profiles-daemon.enable = true;

    # battery info
    upower = {
      enable = true;
      percentageLow = 20;
      percentageCritical = 10;
      percentageAction = 5;
      criticalPowerAction = "Hibernate";
    };
  };

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez5-experimental;
    settings = {
      # make Xbox Series X controller work
      General = {
        Experimental = true;
        FastConnectable = true;
        powerOnBoot = true;
        JustWorksRepairing = "always";
        Privacy = "device";
      };
    };
  };

  boot.extraModprobeConfig = ''options bluetooth disable_ertm=1 '';
  systemd.user.services.telephony_client.enable = false;

  system = {
    stateVersion = "24.11";
  };
}
