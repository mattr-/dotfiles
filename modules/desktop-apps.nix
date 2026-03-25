{ ... }:
{
  flake.modules.homeManager.desktop-apps = { config, pkgs, lib, ... }: {
    config = lib.mkIf config.gui.enable {
      home.packages = with pkgs; [
        discord
        obsidian
        spotify
      ];
    };
  };
}
