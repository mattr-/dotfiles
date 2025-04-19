{
  description = "The mattr- configuration flake";

  inputs = {
    # Nixpkgs
    nixpkgs.url = "git+https://github.com/NixOS/nixpkgs?shallow=1&ref=nixos-unstable";

    # Home manager
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";

    # Flake parts
    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    # Flake utils
    flake-utils.url = "github:numtide/flake-utils";

    # Disko for declarative disk partitioning
    disko.url = "github:nix-community/disko";
    disko.inputs.nixpkgs.follows = "nixpkgs";

    # Modded minecraft server management
    minecraft-servers.url = "github:mkaito/nixos-modded-minecraft-servers";

    # Hardware support
    hardware.url = "github:NixOS/nixos-hardware";
  };

  outputs = inputs @ { self, nixpkgs, flake-parts, flake-utils, ...}:
    flake-parts.lib.mkFlake { inherit inputs; } {
      # Define the systems you want to support
      systems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];

      # Use perSystem to define outputs that vary by system
      perSystem = { config, self', inputs', pkgs, system, ... }: {
        # Your custom packages for each system
        packages = import ./pkgs pkgs;

        # Formatter for each system
        formatter = pkgs.alejandra;

        # You could potentially define devShells here too
        # devShells.default = pkgs.mkShell { ... };
      };

      # Define outputs that are the same across all systems
      flake = {
        # Your custom packages and modifications, exported as overlays
        overlays = import ./overlays {inherit inputs;}; # Pass inputs if overlays need them

        # Reusable nixos modules
        nixosModules = import ./modules/nixos; # Pass inputs if modules need them

        # Reusable home-manager modules
        homeManagerModules = import ./modules/home-manager; # Pass inputs if modules need them

        # NixOS configurations
        nixosConfigurations = {
          prodnose = nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs; } // { outputs = self; }; # Pass self as outputs if needed
            modules = [
              ./hosts/prodnose
            ];
          };
          slugworth = nixpkgs.lib.nixosSystem {
             specialArgs = { inherit inputs; } // { outputs = self; };
             modules = [
               ./hosts/slugworth
               inputs.home-manager.nixosModules.home-manager {
                 home-manager.useUserPackages = true;
                 home-manager.users.mattr- = import ./home-manager/linux/home.nix;
                 # Pass self as outputs if needed by home.nix
                 home-manager.extraSpecialArgs = { inherit inputs; } // { outputs = self; };
               }
             ];
           };
          teevee = nixpkgs.lib.nixosSystem {
            specialArgs = { inherit inputs; } // { outputs = self; };
            modules = [
              ./hosts/teevee
            ];
          };
          knid = nixpkgs.lib.nixosSystem {
             specialArgs = { inherit inputs; } // { outputs = self; };
             modules = [
               ./hosts/knid
               inputs.home-manager.nixosModules.home-manager {
                 home-manager.useUserPackages = true;
                 home-manager.users.mattr- = import ./home-manager/linux/home.nix;
                 home-manager.extraSpecialArgs = { inherit inputs; } // { outputs = self; };
               }
             ];
           };
          violet = nixpkgs.lib.nixosSystem {
             specialArgs = { inherit inputs; } // { outputs = self; };
             modules = [
               ./hosts/violet
               inputs.home-manager.nixosModules.home-manager {
                 home-manager.useUserPackages = true;
                 home-manager.users.mattr- = import ./home-manager/linux/home.nix;
                 home-manager.extraSpecialArgs = { inherit inputs; } // { outputs = self; };
               }
             ];
           };
        };
        # Potentially add home-manager configurations if not managed through NixOS
        # homeConfigurations = { ... };
      };
    };
}

