{
  pkgs,
  config,
  ...
}: {
  console = {
    earlySetup = false;
  };

  boot = {
    plymouth = {
      enable = true;
    };
    kernelParams = [
      "quiet"
      "splash"
      "boot.shell_on_fail"
      "i915.fastboot=1"
      "loglevel=3"
      "udev.log_level=3"
    ];
    consoleLogLevel = 0;
    initrd.verbose = false;
  };
}
