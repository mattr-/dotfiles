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

  flake.modules.darwin.desktop-apps = {
    homebrew = {
      enable = true;
      taps = [ "jurplel/tap" ];
      casks = [
        "1password"
        "alfred"
        "anki"
        "choosy"
        "dash"
        "docker-desktop"
        "elgato-control-center"
        "elgato-stream-deck"
        "firefox"
        "ghostty"
        "hammerspoon"
        "instant-space-switcher"
        "istat-menus"
        "anomaly-hex"
        "microsoft-edge"
        "moonlight"
        "obsidian"
        "omniwm"
        "orbstack"
        "raycast"
        "spotify"
        "steam"
        "tailscale-app"
        "utm"
        "visual-studio-code"
        "vlc"
        "wezterm"
      ];
    };
  };
}
