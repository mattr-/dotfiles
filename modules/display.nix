{ lib, ... }:
{
  flake.modules.nixos.display = { lib, config, ... }:
    let
      cfg = config.dots.display;
    in
    {
      options.dots.display = {
        scale = lib.mkOption {
          type = lib.types.number;
          default = 1;
          description = "Scaling factor for graphical Wayland displays";
        };
      };

      config = {
        hm.dots.display.scale = cfg.scale;
      };
    };

  flake.modules.homeManager.display = {
    options.dots.display = {
      scale = lib.mkOption {
        type = lib.types.number;
        default = 1;
        description = "Scaling factor for graphical Wayland displays";
      };
    };
  };
}
