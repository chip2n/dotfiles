{ config, pkgs, ... }:

{
  home.username = "chip";
  home.homeDirectory = "/home/chip";
  home.sessionPath = [ "$HOME/.local/bin" ];

  home.packages = with pkgs; [
    direnv
    ripgrep
    synergy
    rofi
    polybar
    (st.overrideAttrs (oldAttrs: rec {
      src = fetchFromGitHub {
        owner = "chip2n";
        repo = "st";
        rev = "af159459d8d7edf99bdc40ce15ba37867ea5dd06";
        sha256 = "053m8jxv3l3d8hzx7xc8yx5r25wv003qsiwyhhq9z2qqnl7gabf8";
      };
    }))

    # TODO Refactor this and rc file into separate package?
    sbcl

    # TODO Refactor into polybar module
    zscroll
    playerctl

    iosevka
    jetbrains-mono
  ];

  fonts.fontconfig.enable = true;

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
      package = pkgs.gnome.adwaita-icon-theme;
    };
    cursorTheme = {
      name = "Adwaita";
      package = pkgs.gnome.adwaita-icon-theme;
    };
  };

  programs.firefox = {
    enable = true;
    profiles.default = {
      name = "Default";
      userChrome = builtins.readFile ../firefox/userChrome.css;
      userContent = builtins.readFile ../firefox/userContent.css;
      settings = {
        "browser.newtabpage.activity-stream.feeds.telemetry" = false;
        "browser.newtabpage.activity-stream.telemetry" = false;
        "browser.ping-centre.telemetry" = false;
        "datareporting.healthreport.service.enabled" = false;
        "datareporting.healthreport.uploadEnabled" = false;
        "datareporting.policy.dataSubmissionEnabled" = false;
        "datareporting.sessions.current.clean" = true;
        "devtools.onboarding.telemetry.logged" = false;
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

  programs.bash = {
    enable = true;
    initExtra = builtins.readFile ../.bashrc;
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
  xdg.configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink "${config.home.homeDirectory}/dev/dotfiles/.emacs.d";

  home.file.".background-image".source = ../.background-image;

  # TODO pull quicklisp?
  home.file.".sbclrc".source = ../.sbclrc;

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
  xdg.configFile.polybar.source = ../.config/polybar;

  home.stateVersion = "24.05";

  # Let home manager install and manage itself
  programs.home-manager.enable = true;
}
