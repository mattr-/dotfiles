{ ... }:
{
  flake.modules.nixos.sddm = { config, pkgs, lib, ... }:
    let
      sddm-theme = pkgs.sddm-astronaut.override {
        embeddedTheme = "black_hole";
      };
    in
    {
      config = lib.mkIf config.gui.enable {
        environment.systemPackages = [ sddm-theme ];

        services.displayManager.sddm = {
          enable = true;
          wayland.enable = true;
          theme = "sddm-astronaut-theme";
          extraPackages = [ sddm-theme ];
          settings.General.GreeterEnvironment = "QT_SCREEN_SCALE_FACTORS=${toString config.dots.display.scale}";
        };
      };
    };
}
