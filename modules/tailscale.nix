{ ... }:
{
  flake.modules.nixos.tailscale = {
    services.tailscale = {
      enable = true;
      openFirewall = true;
    };

    networking.firewall.trustedInterfaces = [ "tailscale0" ];
  };
}
