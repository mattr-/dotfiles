{ config, pkgs, ... }:
{
  homebrew = {
    enable = true;
    casks = [
      "1password"
      "1password-cli"
      "alfred"
      "bambu-studio"
      "dash"
      "discord"
      "elgato-control-center"
      "elgato-stream-deck"
      "fantastical"
      "hammerspoon"
      "istat-menus"
      "obs"
      "obsidian"
      "prismlauncher"
      "raycast"
      "slack"
      "spotify"
      "steam"
      "temurin@17"
      "temurin@8"
      "utm"
      "viscosity"
      "visual-studio-code"
      "vivaldi"
      "vlc"
      "zoom"
    ];
  };
}
