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
      "ghostty"
      "hammerspoon"
      "istat-menus"
      "obs"
      "obsidian"
      "prismlauncher"
      "raycast"
      "spotify"
      "steam"
      "temurin@17"
      "temurin@8"
      "todoist"
      "utm"
      "viscosity"
      "visual-studio-code"
      "vivaldi"
      "vlc"
      "wezterm"
    ];
  };
}
