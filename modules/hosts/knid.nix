{ inputs, config, ... }:
let
  nixosModules = builtins.attrValues (config.flake.modules.nixos or { });
  hmModules = builtins.attrValues (config.flake.modules.homeManager or { });
in
{
  flake.nixosConfigurations."knid" = inputs.nixpkgs.lib.nixosSystem {
    system = "x86_64-linux";
    specialArgs = { inherit inputs; };
    modules = nixosModules ++ [
      inputs.hardware.nixosModules.framework-13-7040-amd
      inputs.disko.nixosModules.disko

      ./knid/_hardware-configuration.nix
      ./knid/_disko.nix

      ({ pkgs, ... }: {
        nixpkgs.hostPlatform = "x86_64-linux";

        hardware.gpu = "amd";
        keyd.enable = true;
        dots.display.scale = 2;

        networking.hostName = "knid";

        hardware.brillo.enable = true;
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
