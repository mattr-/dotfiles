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

  # Don't require password for sudo
  security.sudo.wheelNeedsPassword = false;

  # Allow docker containers inside the dev vm
  virtualisation.docker.enable = true;

  system.stateVersion = "24.05";
}
