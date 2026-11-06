{ ... }:
{
  flake.modules.nixos.desktop-apps = { config, pkgs, lib, ... }: {
    config = lib.mkIf config.gui.enable {
      environment.systemPackages = with pkgs; [
        discord
        obsidian
        spotify
        yt-dlp
      ];
    };
  };
}
