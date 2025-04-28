{ self, lib, inputs, ... }:
let
  hostname = builtins.baseNameOf ./.;
  inherit (inputs.nixpkgs.lib) nixosSystem;
in {
  flake.nixosConfigurations = {
    ${hostname} = nixosSystem {
      system = "x86_64-linux";
      specialArgs = { inherit inputs; } // { outputs = self; };
      modules = [
        # Import the host configuration
        ./configuration.nix

        # # Include home-manager as a module
        # inputs.home-manager.nixosModules.home-manager
        # {
        #   home-manager.useGlobalPkgs = true;
        #   home-manager.useUserPackages = true;
        #   home-manager.extraSpecialArgs = { inherit inputs; };
        #   home-manager.users.mattr = import ../../home/users/mattr.nix;
        # }
      ];
    };
  };
}
