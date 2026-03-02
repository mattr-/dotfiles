{ ... }:
{
  flake.modules.nixos.keyd = { config, lib, ... }: {
    options.dots.keyd.enable = lib.mkEnableOption "Keyboard remapping with keyd";

    config = lib.mkIf config.dots.keyd.enable {
      services.keyd = {
        enable = true;
        keyboards.default = {
          ids = [ "*" ];
          settings = {
            main.capslock = "overload(control, esc)";
            global.overload_tap_timeout = 250;
          };
        };
      };
    };
  };
}
