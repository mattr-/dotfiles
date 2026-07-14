{ inputs, config, ... }:
let
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.nixosConfigurations."teevee" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = nixosModules ++ [
      inputs.hardware.nixosModules.common-cpu-intel-cpu-only
      inputs.hardware.nixosModules.common-pc-ssd
      inputs.disko.nixosModules.disko

      ./teevee/_hardware-configuration.nix
      ./teevee/_disko.nix

      ({ pkgs, config, ... }: {
        nixpkgs.hostPlatform = "x86_64-linux";
        networking.hostName = "teevee";
        hardware.gpu = "nvidia";
        gui.enable = false;
        hardware.enableRedistributableFirmware = true;

        dots.minecraft.servers.start-theta-1.enable = true;

        hm.imports = hmModules;
        hm.gui.enable = config.gui.enable;
      })
    ];
  };
}
