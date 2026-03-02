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

      ./knid/_hardware-configuration.nix
      ./knid/_disko.nix

      ({ pkgs, ... }: {
        nixpkgs.hostPlatform = "x86_64-linux";

        hardware.gpu = "amd";

        networking.hostName = "knid";

        dots = {
          audio.enable = true;
          bluetooth.enable = true;
          flatpak.enable = true;
          fonts.enable = true;
          firefox.enable = true;
          gaming.enable = true;
          gnome.enable = true;
          graphics.enable = true;
          keyd.enable = true;
          moonlight.enable = true;
          power.enable = true;
          sunshine.enable = true;
          wayland.enable = true;
          xdg.enable = true;
        };

        hardware.brillo.enable = true;
        hardware.enableRedistributableFirmware = true;
        virtualisation.waydroid.enable = true;
        programs.niri.enable = true;

        environment.systemPackages = with pkgs; [
          quickshell
          matugen
          ddcutil
          khal
          inputs.noctalia.packages.${pkgs.stdenv.hostPlatform.system}.default
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.claude-code
          inputs.llm-agents.packages.${pkgs.stdenv.hostPlatform.system}.opencode
        ];

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
