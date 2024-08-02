{inputs, ...}:
{
  imports = [
    ./hardware-configuration.nix
    ../_global/linux
    ../_global/linux/systemd-boot.nix
    ../../users/mattr-/nixos.nix
  ];

  networking = {
    hostName = "violet";
  };

  system.stateVersion = "24.05";
}
