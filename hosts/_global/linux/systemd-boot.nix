{
  boot.loader = {
    systemd-boot = {
      enable = true;
      consoleMode = "max";
      configurationLimit = 5;
    };
    efi.canTouchEfiVariables = true;
  };
}
