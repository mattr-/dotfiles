{ inputs, config, ... }:
let
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.nixosConfigurations."slugworth" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = nixosModules ++ [
      inputs.hardware.nixosModules.common-cpu-intel-cpu-only
      inputs.hardware.nixosModules.common-pc-ssd
      inputs.hardware.nixosModules.common-gpu-amd
      inputs.disko.nixosModules.disko

      ./slugworth/_hardware-configuration.nix
      ./slugworth/_disko.nix

      ({ pkgs, ... }: {
        nixpkgs.hostPlatform = "x86_64-linux";

        hardware.gpu = "amd";
        keyd.enable = true;
        dots.display.scale = 2;

        networking.hostName = "slugworth";

        hardware.enableRedistributableFirmware = true;
        virtualisation.waydroid.enable = true;
        programs.niri.enable = true;

        environment.systemPackages = with pkgs; [
          quickshell
          matugen
          ddcutil
          khal
          plannotator
          inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.opencode
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.pi
        ];

        hm.imports = hmModules;
      })
    ];
  };
}
