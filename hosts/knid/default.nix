{inputs, outputs, pkgs, ...}: {
  imports = [
    inputs.hardware.nixosModules.framework-13-7040-amd
    ./hardware-configuration.nix
    ../_global/linux
    ../_global/linux/graphical/greetd.nix
    ../_global/linux/graphical/hyprland.nix
    ../_global/linux/steam.nix
    ../_global/linux/sysctl.nix
    ../_global/linux/systemd-boot.nix
    ../../users/mattr-/nixos.nix
  ];

  networking = {
    hostName = "knid";
  };

  hardware.brillo.enable = true;

  programs.niri.enable = true;

  powerManagement = {
    enable = true;
  };

  services = {
    logind = {
      powerKey = "suspend";
      lidSwitch = "suspend";
      lidSwitchExternalPower = "lock";
    };

    power-profiles-daemon.enable = true;

    # battery info
    upower = {
      enable = true;
      percentageLow = 20;
      percentageCritical = 10;
      percentageAction = 5;
      criticalPowerAction = "Hibernate";
    };
  };

  hardware.bluetooth = {
    enable = true;
    package = pkgs.bluez5-experimental;
    settings = {
      # make Xbox Series X controller work
      General = {
        Experimental = true;
        FastConnectable = true;
        powerOnBoot = true;
        JustWorksRepairing = "always";
        Privacy = "device";
      };
    };
  };

  security = {
    # allow wayland lockers to unlock the screen
    pam.services.hyprlock.text = "auth include login";

    # userland niceness
    rtkit.enable = true;
  };

  boot.extraModprobeConfig = ''options bluetooth disable_ertm=1 '';
  systemd.user.services.telephony_client.enable = false;

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
    extraPackages = with pkgs; [
      libva
      vaapiVdpau
      libvdpau-va-gl
      amdvlk
      mesa
    ];
    extraPackages32 = with pkgs.pkgsi686Linux; [
      vaapiVdpau
      libvdpau-va-gl
      amdvlk
    ];
  };

  fonts = {
    packages = with pkgs; [
      # icon fonts
      material-symbols

      # normal fonts
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji

      inputs.self.packages.${pkgs.system}.SF-Pro
      inputs.self.packages.${pkgs.system}.SF-Mono

      # nerdfonts
      nerd-fonts.symbols-only
      nerd-fonts.departure-mono
      departure-mono
    ];

    # causes more issues than it solves
    enableDefaultPackages = false;

    fontconfig = {
      enable = true;
      antialias = true;
      hinting = {
        enable = true;
        autohint = false;
        style = "full";
      };
      subpixel = {
        lcdfilter = "default";
        rgba = "rgb";
      };
      defaultFonts = let
        addAll = builtins.mapAttrs (_: v: ["Symbols Nerd Font"] ++ v ++ ["Noto Color Emoji"]);
      in
        addAll {
          serif = ["Noto Sans Serif"];
          sansSerif = ["SF Pro"];
          monospace = ["SF Mono"];
          emoji = ["Noto Color Emoji"];
        };
    };
    fontDir = {
      enable = true;
      decompressFonts = true;
    };
  };

  services.flatpak.enable = true;

  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    config = {
      common = {
        default = ["gnome" "gtk"];
        "org.freedesktop.impl.portal.ScreenCast" = "gnome";
        "org.freedesktop.impl.portal.Screenshot" = "gnome";
        "org.freedesktop.impl.portal.RemoteDesktop" = "gnome";
      };
    };
    extraPortals = [
      pkgs.xdg-desktop-portal-gtk
      pkgs.xdg-desktop-portal-gnome
    ];
  };

  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    jack.enable = true;
    pulse.enable = true;

    wireplumber = {
      enable = true;
      configPackages = [
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

  system = {
    stateVersion = "24.11";
  };
}
