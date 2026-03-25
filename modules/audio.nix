{ ... }:
{
  flake.modules.nixos.audio = { pkgs, ... }: {
    security.rtkit.enable = true;
    services.pulseaudio.enable = false;

    services.pipewire = {
      enable = true;
      alsa.enable = true;
      alsa.support32Bit = true;
      pulse.enable = true;
      jack.enable = true;

      wireplumber = {
        enable = true;
        configPackages = [
          (pkgs.writeTextDir "share/bluetooth.lua.d/51-bluez-config.lua" ''
            bluez_monitor.properties = {
              ["bluez5.enable-sbc-xq"] = true,
              ["bluez5.enable-msbc"] = true,
              ["bluez5.enable-hw-volume"] = true,
              ["bluez5.headset-roles"] = "[ hsp_hs hsp_ag hfp_hf hfp_ag ]",
              ["bluez5.a2dp.ldac.quality"] = "auto",
              ["bluez5.a2dp.aac.bitratemode"] = 0,
              ["bluez5.default.rate"] = 48000,
              ["bluez5.default.channels"] = 2,
              ["bluez5.headset-profile"] = "a2dp-only"
            }
          '')
        ];
        extraConfig."wireplumber.profiles".main."monitor.libcamera" = "disabled";
      };
    };

    systemd.user.services.telephony_client.enable = false;
  };
}
