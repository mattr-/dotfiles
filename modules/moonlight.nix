{ ... }:
{
  flake.modules.nixos.moonlight = { pkgs, ... }: {
    environment.systemPackages = with pkgs; [
      moonlight-qt
    ];
  };
}
