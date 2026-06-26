{ inputs, ... }:
{
  flake.modules.nixos.home-manager-integration = { lib, ... }: {
    imports = [
      inputs.home-manager.nixosModules.home-manager

      {
        home-manager = {
          backupFileExtension = "bak";
          extraSpecialArgs = { inherit inputs; };
          useGlobalPkgs = true;
          useUserPackages = true;
        };
      }

      (lib.mkAliasOptionModule [ "hm" ] [ "home-manager" "users" "mattr-" ])
    ];
  };

  flake.modules.darwin.home-manager-integration = { lib, ... }: {
    imports = [
      inputs.home-manager.darwinModules.home-manager

      {
        home-manager = {
          useGlobalPkgs = true;
          useUserPackages = true;
          extraSpecialArgs = { inherit inputs; };
        };
      }

      (lib.mkAliasOptionModule [ "hm" ] [ "home-manager" "users" "mattr-" ])
    ];
  };
}
