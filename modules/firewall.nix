{ ... }:
{
  flake.modules.nixos.firewall = {
    networking.nftables.enable = true;

    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 22 ];
    };
  };
}
