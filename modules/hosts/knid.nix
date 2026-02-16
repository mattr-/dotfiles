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
      inputs.home-manager.nixosModules.home-manager
      inputs.hardware.nixosModules.framework-13-7040-amd
      inputs.disko.nixosModules.disko

      # Machine-specific hardware/disko (manually imported from subdirectory)
      ./knid/_hardware-configuration.nix
      ./knid/_disko.nix

      # Host-specific configuration
      ({ pkgs, ... }: {
        nixpkgs.hostPlatform = "x86_64-linux";

        # Meta options (no prefix)
        hardware.gpu = "amd";

        # Hostname
        networking.hostName = "knid";

        # Enable additional features
        hardware.brillo.enable = true;
        hardware.enableRedistributableFirmware = true;
        virtualisation.waydroid.enable = true;
        programs.niri.enable = true;

        # Host-specific packages
        environment.systemPackages = with pkgs; [
          quickshell
          matugen
          ddcutil
          khal
          inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.opencode
        ];

        # Home-manager integration
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = { inherit inputs; };
          users."mattr-" = { ... }: {
            imports = hmModules;
          };
        };
      })
    ];
  };
}
