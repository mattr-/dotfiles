{ ... }:
{
  flake.modules.nixos.firefox = { config, lib, pkgs, ... }: {
    options.dots.firefox.enable = lib.mkEnableOption "Firefox browser";

    config = lib.mkIf config.dots.firefox.enable {
      programs.firefox = {
        enable = true;
        package = pkgs.firefox;
      };
    };
  };

  flake.modules.homeManager.firefox = { ... }: {
    programs.firefox = {
      enable = true;
      profiles.default = {
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
}
