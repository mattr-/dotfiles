{ ... }:
{
  flake.modules.nixos.keyd = { config, lib, ... }: {
    config = lib.mkIf config.keyd.enable {
      services.keyd = {
        enable = true;
        keyboards = {
          default = {
            ids = [ "*" ];
            settings = {
              main = {
                capslock = "overload(control, esc)";
              };
              global = {
                overload_tap_timeout = 250;
              };
            };
          };
        };
      };
    };
  };
}
