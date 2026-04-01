{ ... }:
{
  flake.modules.nixos.sddm = { config, pkgs, ... }:
    let
      sddm-theme = pkgs.sddm-astronaut.override {
        embeddedTheme = "pixel_sakura";
      };
    in
    {
      environment.systemPackages = [ sddm-theme ];

      services.displayManager.sddm = {
        enable = true;
        wayland.enable = true;
        theme = "sddm-astronaut-theme";
        extraPackages = [ sddm-theme ];
        settings.General.GreeterEnvironment = "QT_SCREEN_SCALE_FACTORS=${toString config.dots.display.scale}";
      };

      nixpkgs.overlays = [
        (_: prev: {
          weston = prev.weston.override { vncSupport = false; };
        })
      ];
    };
}
