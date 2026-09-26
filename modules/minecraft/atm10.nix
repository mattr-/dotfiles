{ ... }:
{
  flake.modules.nixos.minecraft-atm10 = { lib, config, moddedMinecraft, ... }: {
    options.dots.minecraft.servers.atm10.enable =
      lib.mkEnableOption "All the Mods 10 Minecraft server";

    config = lib.mkIf config.dots.minecraft.servers.atm10.enable {
      services.modded-minecraft-servers.instances.atm10 =
        moddedMinecraft.mkServer {
          jvmMaxAllocation = "16G";
          jvmPackage = moddedMinecraft.jre21;

          serverConfig = {
            server-port = 25565;
            rcon-port = 25566;
            motd = "Welcome to ATM 10!";
            allow-flight = true;
            pvp = false;

            extra-options = {
              difficulty = "hard";
              gamemode = "survival";
            };
          };
        };
    };
  };
}
