{
  pkgs,
  ...
}: {
  services = {
    transmission = 
      {
        enable = true;
        package = pkgs.transmission_4;
      }
  };
}
