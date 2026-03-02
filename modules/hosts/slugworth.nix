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
      inputs.home-manager.nixosModules.home-manager
      inputs.hardware.nixosModules.common-cpu-intel-cpu-only
      inputs.hardware.nixosModules.common-pc-ssd
      inputs.hardware.nixosModules.common-gpu-amd
      inputs.disko.nixosModules.disko

      ./slugworth/_hardware-configuration.nix
      ./slugworth/_disko.nix

      ({ pkgs, ... }: {
        nixpkgs.hostPlatform = "x86_64-linux";

        hardware.gpu = "amd";

        networking.hostName = "slugworth";

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

        programs.sway.enable = true;
        programs.niri.enable = true;
        virtualisation.waydroid.enable = true;

        security.pam.services.hyprlock.text = "auth include login";

        environment.systemPackages = with pkgs; [
          quickshell
          matugen
          ddcutil
          khal
          plannotator
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
