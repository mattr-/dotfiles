{
  pkgs,
  config,
  lib,
  ...
}: let
  windows_space_gap = 15;
in {
  programs.rofi.enable = true;
  home.packages = with pkgs; [
    hyprshot
    brightnessctl
    wl-clipboard
    wlsunset
    swww
  ];

  services = {
    hypridle = {
      enable = true;
      settings = {
        general = {
          lock_cmd = "echo locking";
          before_sleep_cmd = "echo 'going to sleep'";
          after_sleep_cmd = "echo 'waking up from sleep'";
        };
        listener = [
          {
            timeout = 300;
            on-timeout = "hyprctl dispatch dpms off";
            on-resume = "hyprctl dispatch dpms on";
          }
        ];
      };
    };
  };

  wayland.windowManager.hyprland = {
    enable = true;
    plugins = with pkgs.hyprlandPlugins; [
      hyprtrails
      hyprwinwrap
    ];
    settings = {
      "$terminal" = "ghostty";
      "$menu" = "rofi -show drun";
      "exec-once" = [
        "1password --silent"
        "$terminal"
        "waybar"
        "wlsunset -S 7:00 -s 23:30"
        "swayosd-server"
      ];

      "env" = [
        "XCURSOR_SIZE,24"
        "HYPRCURSOR_SIZE,24"
        "QT_AUTO_SCREEN_SCALE_FACTOR,1"
        "QT_QPA_PLATFORM,wayland;xcb"
        "QT_WAYLAND_DISABLE_WINDOWDECORATION,1"
      ];
      debug = {
        disable_logs = false;
      };

      general = {
        gaps_in = 5;
        gaps_out = windows_space_gap;
        border_size = 2;
        resize_on_border = false;
        allow_tearing = false;
        layout = "dwindle";
      };

      decoration = {
        rounding = 10;
        active_opacity = 1.0;
        inactive_opacity = 1.0;

        shadow.range = 4;
        shadow.render_power = 3;

        blur = {
          enabled = true;
          size = 3;
          passes = 1;
          vibrancy = 0.1696;
        };
      };

      animations = {
        enabled = true;
        bezier = "myBezier, 0.05, 0.9, 0.1, 1.05";

        animation = [
          "windows, 1, 7, myBezier"
          "windowsOut, 1, 7, default, popin 80%"
          "border, 1, 10, default"
          "borderangle, 1, 8, default"
          "fade, 1, 7, default"
          "workspaces, 1, 6, default"
        ];
      };
      dwindle = {
        pseudotile = true;
        preserve_split = true;
      };

      master = {
        new_status = "master";
      };

      misc = {
        force_default_wallpaper = 0;
        disable_hyprland_logo = true;
      };

      input = {
        kb_layout = "us";
        kb_variant = "";
        kb_model = "";
        kb_options = "";
        kb_rules = "";

        follow_mouse = 1;

        sensitivity = 0;

        touchpad = {
          natural_scroll = false;
        };
      };

      gestures = {
        workspace_swipe = false;
      };

      device = {
        name = "epic-mouse-v1";
        sensitivity = -0.5;
      };
      "$mainMod" = "SUPER";

      bind = [
        "$mainMod, Q, exec, $terminal"
        "$mainMod, W, killactive,"
        "$mainMod CTRL, Q, exit,"
        "$mainMod, V, togglefloating,"
        "$mainMod, Space, exec, $menu"
        "$mainMod, P, pseudo,"
        "$mainMod, J, togglesplit,"
        "$mainMod, F, fullscreen, 1"
        "$mainMod, M, fullscreen, 0"
        ", Print, exec, hyprshot -m region"
        "$mainMod, Print, exec, hyprshot -m output"

        "$mainMod, L, exec, hyprlock"

        # Move focus with mainMod + arrow keys
        "$mainMod, left, movefocus, l"
        "$mainMod, right, movefocus, r"
        "$mainMod, up, movefocus, u"
        "$mainMod, down, movefocus, d"

        # Switch workspaces with mainMod + [0-9]
        "$mainMod, 1, workspace, 1"
        "$mainMod, 2, workspace, 2"
        "$mainMod, 3, workspace, 3"
        "$mainMod, 4, workspace, 4"
        "$mainMod, 5, workspace, 5"
        "$mainMod, 6, workspace, 6"
        "$mainMod, 7, workspace, 7"
        "$mainMod, 8, workspace, 8"
        "$mainMod, 9, workspace, 9"
        "$mainMod, 0, workspace, 10"

        # Move active window to a workspace with mainMod + SHIFT + [0-9]
        "$mainMod SHIFT, 1, movetoworkspace, 1"
        "$mainMod SHIFT, 2, movetoworkspace, 2"
        "$mainMod SHIFT, 3, movetoworkspace, 3"
        "$mainMod SHIFT, 4, movetoworkspace, 4"
        "$mainMod SHIFT, 5, movetoworkspace, 5"
        "$mainMod SHIFT, 6, movetoworkspace, 6"
        "$mainMod SHIFT, 7, movetoworkspace, 7"
        "$mainMod SHIFT, 8, movetoworkspace, 8"
        "$mainMod SHIFT, 9, movetoworkspace, 9"
        "$mainMod SHIFT, 0, movetoworkspace, 10"

        # Example special workspace (scratchpad)
        "$mainMod, S, togglespecialworkspace, magic"
        "$mainMod SHIFT, S, movetoworkspace, special:magic"

        # Scroll through existing workspaces with mainMod + scroll
        "$mainMod, mouse_down, workspace, e+1"
        "$mainMod, mouse_up, workspace, e-1"
      ];

      bindm = [
        "$mainMod, mouse:272, movewindow"
        "$mainMod, mouse:273, resizewindow"
      ];

      # Laptop multimedia keys for volume and LCD brightness
      bindel = [
        ",XF86AudioRaiseVolume, exec, swayosd-client --output-volume raise"
        ",XF86AudioLowerVolume, exec, swayosd-client --output-volume lower"
        ",XF86AudioMute, exec, swayosd-client --output-volume mute-toggle"
        ",XF86AudioMicMute, exec, swayosd-client --input-volume mute-toggle"

        ",XF86MonBrightnessUp, exec, swayosd-client --brightness raise"
        ",XF86MonBrightnessDown, exec, swayosd-client --brightness lower"
      ];

      # Requires playerctl
      bindl = lib.lists.optionals config.services.playerctld.enable [
        ", XF86AudioNext, exec, playerctl next"
        ", XF86AudioPause, exec, playerctl play-pause"
        ", XF86AudioPlay, exec, playerctl play-pause"
        ", XF86AudioPrev, exec, playerctl previous"
      ];

      windowrulev2 = [
        "suppressevent maximize, class:.*"
        "nofocus,class:^$,title:^$,xwayland:1,floating:1,fullscreen:0,pinned:0"
      ];

    };
  };

  programs.waybar = {
    enable = true;
    style = ''
      * {
        color: #eeeeee;
        font-family: "Iosevka";
        font-weight: bold;
        font-size: 14px;
      }

      window#waybar {
        background-color: rgba(0,0,0,0);
      }

      #waybar > box {
        margin: 10px ${builtins.toString windows_space_gap}px 0px;
        background-color: rgb(100,100,100);
        border: 2px solid rgb(150,150,150);
      }

      #workspaces,
      #window,
      #idle_inhibitor,
      #wireplumber,
      #network,
      #cpu,
      #memory,
      #clock,
      #tray,
      #waybar > box {
        border-radius: 12px;
      }

      #idle_inhibitor,
      #wireplumber,
      #network,
      #cpu,
      #memory,
      #battery,
      #clock,
      #power-profiles-daemon,
      #tray {
        padding: 0 5px;
      }
    '';

    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        spacing = 4;
         modules-left = [
          "hyprland/workspaces"
          "hyprland/window"
        ];
        modules-center = [
          "clock"
        ];
        modules-right = [
          "idle_inhibitor"
          "wireplumber"
          "network"
          "bluetooth"
          "battery"
          "power-profiles-daemon"
          "tray"
        ];

        "hyprland/workspaces" = {
          disable-scroll = true;
          all-outputs = true;
          warp-on-scroll = false;
          format = "{name}: {icon}";
          format-icons = {
            "urgent" = "";
            "active" = "";
            "default" = "";
          };
        };

        idle_inhibitor = {
          format = "Idle: {icon} ";
          format-icons = {
            "deactivated" = "";
            "activated" = "";
          };
        };

        wireplumber = {
          format = "Volume: {icon}  {volume}% ";
          format-icons = ["" "" ""];
          format-muted = "Muted ";
        };

        clock = {
          format = "  {:%H:%M}";
        };

        power-profiles-daemon = {
          format = "Profile: {icon} ";
          tooltip-format = "Power profile: {profile}\nDriver: {driver}";
          tooltip = true;
          format-icons = {
            default = "";
            performance = "";
            balanced = "";
            power-saver = "";
          };
        };
      };
    };
  };
}
