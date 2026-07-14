{ inputs, ... }:
{
  imports = [ ./minecraft/start-theta-1.nix ];

  flake.modules.nixos.minecraft = { pkgs, lib, config, ... }:
    let
      inherit (lib) concatStringsSep;

      rsyncSSHKeys = config.users.users.mattr-.openssh.authorizedKeys.keys;

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
        enable = lib.mkDefault true;
        inherit rsyncSSHKeys jvmOpts;

        jvmPackage = lib.mkDefault pkgs.temurin-bin;
        jvmInitialAllocation = lib.mkDefault "4G";

        serverConfig = {
          white-list = lib.mkDefault false;
          max-tick-time = lib.mkDefault (5 * 60 * 1000);
        };
      };
    in
    {
      imports = [ inputs.minecraft-servers.module ];

      _module.args.moddedMinecraft = {
        jre8 = pkgs.temurin-bin-8;
        jre17 = pkgs.temurin-bin-17;
        jre21 = pkgs.temurin-bin;

        mkServer = overrides:
          lib.mkMerge [
            serverDefaults
            overrides
          ];
      };

      services.modded-minecraft-servers.eula = true;
    };
}
