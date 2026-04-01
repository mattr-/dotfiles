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
}
