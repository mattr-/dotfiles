# built from https://discourse.nixos.org/t/combining-best-of-system-firefox-and-home-manager-firefox-settings/37721
{ config, pkgs, ... }:
let
  lock-false = { Value = false ; Status = "locked"; };
  lock-true = { Value = true; Status = "locked"; };
  lock-empty-string = { Value = ""; Status = "locked"; };
in {
  programs.firefox = {
    enable = true;
    package = unstable.firefox;
    policies = {
      DisableTelemetry = true;
      DisableFirefoxStudies = true;
      DontCheckDefaultBrowser = true;
      DisablePocket = true;
      SearchBar = "unified";

      Preferences = {
        "extensions.pocket.enabled" = lock-false;
        "browser.newtabpage.pinned" = lock-empty-string;
        "browser.topsites.contile.enabled" = lock-false;
        "browser.newtabpage.activity-stream.showSponsored" = lock-false;
        "browser.newtabpage.activity-stream.system.showSponsored" = lock-false;
        "browser.newtabpage.activity-stream.showSponsoredTopSites" = lock-false;
      };

      ExtensionSettings = {
        "uBlock0@raymondhill.net" = {
          install_url = "https://addons.mozilla.org/firefox/downloads/latest/ublock-origin/latest.xpi";
          installation_mode = "force_installed";
        };
      };
    };
  };
}
