{ ... }:
{
  flake.modules.nixos.network = {
    networking.networkmanager = {
      enable = true;
      dns = "systemd-resolved";
    };
    services.resolved = {
      enable = true;
      settings.Resolve.DNSOverTLS = "opportunistic";
    };
  };
}
