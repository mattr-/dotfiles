{ ... }:
{
  flake.modules.nixos.gnome = { pkgs, ... }: {
    services.desktopManager.gnome.enable = true;
    services.displayManager.gdm.enable = true;

    environment.systemPackages = with pkgs; [
      gnomeExtensions.appindicator
      gnomeExtensions.paperwm
    ];

    services.udev.packages = [ pkgs.gnome-settings-daemon ];
    services.gnome.gcr-ssh-agent.enable = false;
    services.orca.enable = false;
  };

  flake.modules.homeManager.gnome = {
    dconf.settings = {
      "org/gnome/desktop/interface" = {
        color-scheme = "prefer-dark";
      };
    };
  };
}

