{ inputs, config, ... }:
let
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.nixosConfigurations."prodnose" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = nixosModules ++ [
      inputs.hardware.nixosModules.common-cpu-intel-cpu-only
      inputs.hardware.nixosModules.common-pc-ssd
      inputs.disko.nixosModules.disko

      ./prodnose/_hardware-configuration.nix
      ./prodnose/_disko.nix

      ({ pkgs, ... }: {
        nixpkgs.hostPlatform = "x86_64-linux";
        networking.hostName = "prodnose";
        hardware.gpu = "nvidia";
        hardware.enableRedistributableFirmware = true;

        hm.imports = hmModules;
      })
    ];
  };
}
