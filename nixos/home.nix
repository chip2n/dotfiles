{ config, pkgs, lib, inputs, ... }:

{
  imports = [
    ./home/zig.nix
  ];

  home.username = "chip";
  home.homeDirectory = "/home/chip";
  home.packages = with pkgs; [
    scrot
    vlc
    deluge-gtk
    unzip
    claude-code
    gnumake
    gcc
    pavucontrol
    pass
    direnv
    vulkan-tools
    killall
    ripgrep
    rofi
    polybar
    ghostty
    (st.overrideAttrs (oldAttrs: rec {
      src = fetchFromGitHub {
        owner = "chip2n";
        repo = "st";
        rev = "af159459d8d7edf99bdc40ce15ba37867ea5dd06";
        sha256 = "053m8jxv3l3d8hzx7xc8yx5r25wv003qsiwyhhq9z2qqnl7gabf8";
      };
    }))
    vlc

    # TODO Refactor into polybar module
    zscroll
    playerctl
  ];

  systemd.user.services.tresorit = {
    Unit = {
      Description = "Tresorit";
      PartOf = [ "graphical-session.target" ];
      After = [ "graphical-session.target" ];
    };
    Service = {
      Type = "forking";
      Environment = [
        "PATH=${lib.makeBinPath [ pkgs.bash pkgs.coreutils ]}"
      ];
      ExecStart = "/home/chip/.local/bin/tresorit_fhs_launcher.sh";
      Restart = "on-failure";
      RestartSec = "5s";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };

  wayland.windowManager.hyprland.enable = true;
  wayland.windowManager.hyprland.settings = {
    "$mod" = "SUPER";
    "$term" = "st";
    bind =
      [
        "$mod, F, exec, firefox"
        "$mod, P, exec, rofi -show run"
        "$mod, Return, exec, $term"
        "$mod, C, killactive"
        "$mod, H, movefocus, l"
        "$mod, I, movefocus, r"
        "$mod, E, movefocus, u"
        "$mod, N, movefocus, d"
        ", Print, exec, grimblast copy area"
      ]
      ++ (
        # workspaces
        # binds $mod + [shift +] {1..9} to [move to] workspace {1..9}
        builtins.concatLists (builtins.genList (i:
            let ws = i + 1;
            in [
              "$mod, code:1${toString i}, workspace, ${toString ws}"
              "$mod SHIFT, code:1${toString i}, movetoworkspace, ${toString ws}"
            ]
          )
          9)
      );
    animations = {
      enabled = false;
    };
    workspace = [
      "1, persistent:true"
      "2, persistent:true"
      "3, persistent:true"
      "4, persistent:true"
      "5, persistent:true"
    ];
    exec-once = [
      "waybar"
    ];
  };

  gtk = {
    enable = true;
    theme = {
      name = "Orchis-Yellow-Dark-Compact";
      package = pkgs.orchis-theme.override {
        tweaks = [
          "solid"
          "compact"
        ];
      };
    };
    iconTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
    cursorTheme = {
      name = "Adwaita";
      package = pkgs.adwaita-icon-theme;
    };
  };

  programs.bash = let
   extraBashConfig = builtins.readFile ../.bashrc;
  in {
    enable = true;
    bashrcExtra = extraBashConfig;
  };

  programs.htop = {
    enable = true;
    settings.show_cpu_temperature = 1;
  };

  programs.git = {
    enable = true;
    userEmail = "andreas@arvidsson.io";
    userName = "chip2n";
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-gtk;
  };
  services.emacs = {
    enable = true;
    defaultEditor = true;
  };
  xdg.configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dev/dotfiles/.emacs.d";

  programs.foot = {
    enable = true;
    settings = {
      main = {
        font = "Iosevka:size=11";
      };
    };
  };

  programs.waybar = {
    enable = true;
    settings = {
      mainBar = {
        layer = "top";
        position = "top";
        height = 30;
        spacing = 8;
        margin-left = 4;
        margin-right = 8;
        modules-left = [ "hyprland/workspaces" ];
        modules-center = ["hyprland/window" ];
        modules-right = [ "memory" "cpu" "clock" ];
        "hyprland/workspaces" = {
          all-outputs = true;
          persistent-workspaces = true;
        };
      };
    };
    style = ''
      * {
        border: none;
        border-radius: 0;
        font-family: Iosevka;
      }
      window#waybar {
        background: #16191C;
        color: #AAB2BF;
      }
      #workspaces button {
        padding: 0 5px;
      }
      #workspaces button.empty {
        color: #808080;
      }
      #workspaces button.active {
        color: #FFFFFF;
      }
    '';
  };

  programs.firefox = {
    enable = true;
    profiles.default = {
      name = "Default";
      userChrome = builtins.readFile ../firefox/userChrome.css;
      userContent = builtins.readFile ../firefox/userContent.css;
      extensions.packages = with pkgs.nur.repos.rycee.firefox-addons; [
        ublock-origin
        tree-style-tab
      ];
      settings = {
        "browser.startup.page" = 3;
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.ping-centre.telemetry" = false;
        "datareporting.healthreport.service.enabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "datareporting.sessions.current.clean" = true;
        "devtools.onboarding.telemetry.logged" = false;
        "toolkit.legacyUserProfileCustomizations.stylesheets" = true;
        "toolkit.telemetry.archive.enabled" = false;
        "toolkit.telemetry.bhrPing.enabled" = false;
        "toolkit.telemetry.enabled" = false;
        "toolkit.telemetry.firstShutdownPing.enabled" = false;
        "toolkit.telemetry.hybridContent.enabled" = false;
        "toolkit.telemetry.newProfilePing.enabled" = false;
        "toolkit.telemetry.prompted" = 2;
        "toolkit.telemetry.rejected" = true;
        "toolkit.telemetry.reportingpolicy.firstRun" = false;
        "toolkit.telemetry.server" = "";
        "toolkit.telemetry.shutdownPingSender.enabled" = false;
        "toolkit.telemetry.unified" = false;
        "toolkit.telemetry.unifiedIsOptIn" = false;
        "toolkit.telemetry.updatePing.enabled" = false;
      };
    };
  };

  # Desktop wallpaper
  home.file.".background-image".source = ../.background-image;

  home.file.".local/bin/launch_polybar.sh" = {
    source = ../scripts/launch_polybar.sh;
    executable = true;
  };
  home.file.".local/bin/polybar_host" = {
    source = ../scripts/polybar_host;
    executable = true;
  };
  home.file.".local/bin/polybar_volume" = {
    source = ../scripts/polybar_volume;
    executable = true;
  };
  home.file.".local/bin/polybar_spotify" = {
    source = ../scripts/polybar_spotify;
    executable = true;
  };
  home.file.".local/bin/song" = {
    source = ../scripts/song;
    executable = true;
  };

  home.file.".config/rofi/config.rasi".text = let
    rofiThemePath = "${pkgs.rofi}/share/rofi/themes/Arc-Dark.rasi";
  in ''
    @theme "${rofiThemePath}"
  '';

  xdg.configFile.polybar.source = ../.config/polybar;

  home.stateVersion = "25.11";

  # Let home manager install and manage itself
  programs.home-manager.enable = true;
}
