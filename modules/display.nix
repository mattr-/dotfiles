{ lib, ... }:
let
  displayOptions = {
    options.dots.display.scale = lib.mkOption {
      type = lib.types.number;
      default = 1;
      description = "Scaling factor for graphical Wayland displays";
    };
  };
in
{
  flake.modules.nixos.display = { config, ... }: {
    imports = [ displayOptions ];

    hm.dots.display.scale = config.dots.display.scale;
  };

  flake.modules.homeManager.display = displayOptions;
}
