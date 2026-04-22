{ ... }:
{
  flake.modules.nixos.firewall = {
    networking.nftables.enable = true;

    networking.firewall = {
      enable = true;
      allowedTCPPorts = [ 22 47984 47989 48010 ];
      allowedUDPPorts = [ 47998 47999 48000 48002 48010 ];
    };
  };
}
