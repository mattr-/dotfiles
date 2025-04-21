{pkgs, ...}: {
  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez5-experimental;
    settings = {
      General = {
        RefreshDiscovery = true;
        KernelExperimental = true;
        Experimental = true;
        FastConnectable = true;
        JustWorksRepairing = "always";
        Privacy = "device";
      };
    };
  };

  boot.extraModprobeConfig = ''options bluetooth disable_ertm=1 '';
  systemd.user.services.telephony_client.enable = false;

  services.pipewire = {
    wireplumber = {
      enable = true;
      configPackages = [
        # Improve the sound quality for BT headphones
        # Check https://wiki.gentoo.org/wiki/User:Nathanlkoch/Tutorials/Audio
        # for additional improvements to make
        (pkgs.writeTextDir "share/bluetooth.lua.d/51-bluez-config.lua" ''
          bluez_monitor.properties = {
            ["bluez5.enable-sbc-xq"] = true,
            ["bluez5.enable-msbc"] = true,
            ["bluez5.enable-hw-volume"] = true,
            ["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]"
            ["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]",
            ["bluez5.a2dp.ldac.quality"] = "auto",
            ["bluez5.a2dp.aac.bitratemode"] = 0,
            ["bluez5.default.rate"] = 48000,
            ["bluez5.default.channels"] = 2,
            ["bluez5.headset-profile"] = "a2dp-only"  # A2DP for better quality
          }
        '')
      ];
      extraConfig."wireplumber.profiles".main."monitor.libcamera" = "disabled";
    };
  };
}
