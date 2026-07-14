{ ... }:
{
  flake.modules.nixos.moonlight = { config, lib, pkgs, ... }: {
    config = lib.mkIf config.gui.enable {
      environment.systemPackages = with pkgs; [
        moonlight-qt
      ];
    };
  };
}
