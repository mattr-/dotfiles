{inputs, outputs, ...}: {
  imports = [
    inputs.hardware.nixosModules.framework.13-inch.7040-amd
    ./hardware-configuration.nix
    ../_global/linux
    ../_global/linux/graphical/greetd.nix
    ../_global/linux/graphical/hyprland.nix
    # ../_global/linux/steam.nix
    # ../_global/linux/sysctl.nix
    ../_global/linux/systemd-boot.nix
    ../../users/mattr-/nixos.nix
  ];

  networking = {
    hostName = "knid";
  };

  programs.sway.enable = true;

  system = {
    stateVersion = "24.11";
  };
}
