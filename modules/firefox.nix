{ ... }:
{
  flake.modules.nixos.firefox = {
    programs.firefox.enable = true;
  };

  flake.modules.homeManager.firefox = { config, lib, ... }: {
    config = lib.mkIf config.gui.enable {
      programs.firefox = {
        enable = true;
        configPath = ".mozilla/firefox";
        profiles = {
          default = {
            id = 0;
            name = "mattr-";
            isDefault = true;
            settings = {
              "browser.search.defaultenginename" = "ddg";
              "browser.search.order.1" = "ddg";
              "signon.rememberSignons" = false;
              "widget.use-xdg-desktop-portal.file-picker" = 1;
              "browser.aboutConfig.showWarning" = false;
              "browser.compactmode.show" = true;
              "browser.cache.disk.enable" = false;
              "widget.disable-workspace-management" = true;
            };
            search = {
              force = true;
              default = "ddg";
              order = [ "ddg" "google" ];
            };
          };
        };
      };
    };
  };
}
