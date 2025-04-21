{pkgs, ...}:
{
  # Use NetworkManager for networking
  networking.networkmanager = {
    enable = true;
    dns = "systemd-resolved";
  };

  services = {
    resolved = {
      enable = true;
      dnsovertls = "opportunistic";
    };
  };
}
