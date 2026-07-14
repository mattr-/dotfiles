{ ... }:
{
  flake.modules.nixos.minecraft-start-theta-1 = { lib, config, moddedMinecraft, ... }: {
    options.dots.minecraft.servers.start-theta-1.enable =
      lib.mkEnableOption "Star Technology - Theta 1 Minecraft server";

    config = lib.mkIf config.dots.minecraft.servers.start-theta-1.enable {
      services.modded-minecraft-servers.instances.start-theta-1 =
        moddedMinecraft.mkServer {
          jvmMaxAllocation = "10G";
          jvmPackage = moddedMinecraft.jre17;

          serverConfig = {
            server-port = 25569;
            rcon-port = 25570;
            motd = "Star Technology - Theta 1";
            allow-flight = true;
            pvp = true;

            extra-options = {
              level-type = "skyblockbuilder:skyblock";
              difficulty = "peaceful";
              gamemode = "survival";
            };
          };
        };
    };
  };
}
