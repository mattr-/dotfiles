{ inputs, config, ... }:
let
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });
in
{
  flake.nixosConfigurations."teevee" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = nixosModules ++ [
      inputs.disko.nixosModules.disko

      ./teevee/_hardware-configuration.nix
      ./teevee/_disko.nix

      ({ pkgs, lib, config, ... }:
        let
          inherit (lib) concatStringsSep;
          rsyncSSHKeys = config.users.users.mattr-.openssh.authorizedKeys.keys;
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
            white-list = false;
            max-tick-time = 5 * 60 * 1000;
          };
        in
        {
          imports = [ inputs.minecraft-servers.module ];

          nixpkgs.hostPlatform = "x86_64-linux";

          networking.hostName = "teevee";

          environment.systemPackages = with pkgs; [
            vim
            curl
            git
            ghostty.terminfo
          ];

          boot.kernel.sysctl = {
            "kernel.sched_cfs_bandwidth_slice_us" = 3000;
            "net.ivp4.tcp_fin_timeout" = 5;
            "kernel.split_lock_mitigate" = 0;
            "vm.max_map_count" = 2147483642;
          };

          services.modded-minecraft-servers = {
            eula = true;
            instances = {
              atm10 = {
                enable = true;
                inherit rsyncSSHKeys jvmOpts;
                jvmMaxAllocation = "16G";
                jvmInitialAllocation = "4G";
                jvmPackage = jre21;
                serverConfig = serverDefaults // {
                  server-port = 25565;
                  rcon-port = 25566;
                  motd = "Welcome to ATM 10!";
                  allow-flight = true;
                  pvp = false;
                  extra-options.difficulty = "hard";
                  extra-options.gamemode = "survival";
                };
              };

              atm10tts = {
                enable = true;
                inherit rsyncSSHKeys jvmOpts;
                jvmMaxAllocation = "16G";
                jvmInitialAllocation = "4G";
                jvmPackage = jre21;
                serverConfig = serverDefaults // {
                  server-port = 25567;
                  rcon-port = 25568;
                  motd = "Welcome to ATM 10 - To The Sky!";
                  allow-flight = true;
                  pvp = false;
                  extra-options.level-type = "skyblockbuilder:skyblock";
                  extra-options.difficulty = "hard";
                  extra-options.gamemode = "survival";
                };
              };
            };
          };
        })
    ];
  };
}
