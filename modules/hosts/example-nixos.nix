{ inputs, config, ... }:
let
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.nixosConfigurations."example-nixos" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = nixosModules ++ [
      {
        nixpkgs.hostPlatform = "x86_64-linux";

        system.stateVersion = "24.11";

        boot.loader.systemd-boot.enable = false;
        boot.loader.grub.enable = true;
        boot.loader.grub.device = "/dev/sda";

        fileSystems."/" = {
          device = "/dev/sda1";
          fsType = "ext4";
        };

        users.users."mattr-" = {
          isNormalUser = true;
          extraGroups = [ "wheel" "networkmanager" ];
          home = "/home/mattr-";
        };

        hm = { ... }: {
          imports = hmModules;
          home.stateVersion = "24.11";
        };
      }
    ];
  };
}
