{inputs, pkgs, ...}:
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

  # Too many things are marked as broken on aarch64. :(
  nixpkgs.config.allowUnsupportedSystem = true;

  # Allow docker containers inside the dev vm
  virtualisation.docker.enable = true;

  # Set up vmware guest tools
  # VMWare and Parallels both only support this being 0
  boot.loader.systemd-boot.consoleMode = "0";

  programs.sway.enable = true;

  security.wrappers.fbterm = {
    owner = "root";
    group = "video";
    capabilities = "cap_sys_tty_config+ep";
    source = "${pkgs.fbterm}/bin/fbterm";
  };

  services.greetd = {
    enable = true;
    settings = {
      default_session = {
        command = "${pkgs.greetd.greetd}/bin/agreety --cmd /run/wrappers/bin/fbterm";
      };
    };
  };

  system.stateVersion = "24.05";
}
