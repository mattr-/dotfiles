# Most of this is borrowed from
# https://github.com/mkaito/nixos-configurations/blob/cbf3487ed4b14ebd89c97c852743355d2344e895/stargazer/minecraft-servers.nix
{
  pkgs,
  lib,
  inputs,
  config,
  ...
}: let
  inherit (lib) concatStringsSep;
  rsyncSSHKeys = config.users.users.mattr-.openssh.authorizedKeys.keys;
  jre8 = pkgs.temurin-bin-8;
  jre17 = pkgs.temurin-bin-17;
  jre21 = pkgs.temurin-bin;

  jvmOpts = concatStringsSep " " [
    "-XX:+UseG1GC"
    "-XX:+ParallelRefProcEnabled"
    "-XX:MaxGCPauseMillis=200"
    "-XX:+UnlockExperimentalVMOptions"
    "-XX:+DisableExplicitGC"
    "-XX:+AlwaysPreTouch"
    "-XX:G1NewSizePercent=40"
    "-XX:G1MaxNewSizePercent=50"
    "-XX:G1HeapRegionSize=16M"
    "-XX:G1ReservePercent=15"
    "-XX:G1HeapWastePercent=5"
    "-XX:G1MixedGCCountTarget=4"
    "-XX:InitiatingHeapOccupancyPercent=20"
    "-XX:G1MixedGCLiveThresholdPercent=90"
    "-XX:G1RSetUpdatingPauseTimePercent=5"
    "-XX:SurvivorRatio=32"
    "-XX:+PerfDisableSharedMem"
    "-XX:MaxTenuringThreshold=1"
    "-Dfml.readTimeout=120"
  ];

  serverDefaults = {
    white-list = true;
    max-tick-time = 5 * 60 * 1000;
  };
in {
  imports = [inputs.minecraft-servers.module];
  services.modded-minecraft-servers = {
    eula = true;
    instances = {
      atm10 = {
        enable = true;
        inherit rsyncSSHKeys jvmOpts;
        jvmMaxAllocation = "16G";
        jvmInitialAllocation = "4G";
        jvmPackage = jre21;
        serverConfig =
          serverDefaults
          // {
            server-port = 25565;
            rcon-port = 25566;
            motd = "Welcome to ATM 10!";
            allow-flight = true;
            pvp = false;
            extra-options.difficulty = "hard";
            extra-options.gamemode = "survival";
        };
      };
    };
  };
}
