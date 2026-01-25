{ config, pkgs, inputs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
      ./audio.nix
      ./wm/xmonad.nix
      inputs.home-manager.nixosModules.default
      inputs.sops-nix.nixosModules.sops
    ];

  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = "/home/chip/.config/sops/age/keys.txt";
  sops.secrets.openai = {};
  sops.secrets.anthropic = {};
  sops.templates."emacs_secrets" = {
    content = ''
      ;; openai
      (setq private/openai-key "${config.sops.placeholder.openai}")

      ;; anthropic
      (setq private/anthropic-key "${config.sops.placeholder.anthropic}")

      (provide 'private)
    '';
    path = "/home/chip/.config/emacs/src/private.el";
    owner = "chip";
  };

  environment.localBinInPath = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  networking.hostName = "chipb0x";
  networking.networkmanager.enable = true;

  networking.firewall.enable = true;
  # Open port 24800 for Synergy
  networking.firewall.allowedTCPPorts = [ 24800 ];

  time.timeZone = "Europe/Stockholm";

  i18n.defaultLocale = "en_US.UTF-8";

  services.xserver.displayManager.sessionCommands = ''
    xrandr --output DP-2 --mode 2560x1440 --rate 143.91
    xrandr --output DP-3 --mode 2560x1440 --rate 143.91
    xrandr --output HDMI-1 --mode 2560x1440 --rate 143.91
  '';

  services.xserver = {
    enable = true;
    autoRepeatDelay = 200;
    autoRepeatInterval = 20;
    layout = "us";
    xrandrHeads = [
      {
        output = "DP-2";
      }
      {
        output = "DP-3";
        primary = true;
      }
      {
        output = "HDMI-1";
      }
    ];
  };

  # Enable the KDE Plasma Desktop Environment.
  services.displayManager.sddm.enable = true;
  services.displayManager.autoLogin.enable = true;
  services.displayManager.autoLogin.user = "chip";

  # services.desktopManager.plasma6.enable = true;
  programs.hyprland = {
    enable = true;
    withUWSM = true;
    xwayland.enable = true;
  };

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # Enable sound with pipewire.
  services.pulseaudio.enable = false;
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    # If you want to use JACK applications, uncomment this
    #jack.enable = true;

    # use the example session manager (no others are packaged yet so this is enabled by default,
    # no need to redefine it in your config for now)
    #media-session.enable = true;
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.users.chip = {
    isNormalUser = true;
    description = "chip";
    extraGroups = [ "networkmanager" "wheel" ];
    packages = with pkgs; [
      kdePackages.kate
    ];
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs; [
    # fuse
    git
    vim
    # emacs
    wget
  ];

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "25.11"; # Did you read the comment?

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  fonts.packages = with pkgs; [
    noto-fonts
    iosevka
    siji
  ];
  fonts.fontconfig.enable = true;
  fonts.fontconfig.allowBitmaps = true;

  programs.thunar.enable = true;
  programs.steam.enable = true;

  hardware.graphics = {
    enable = true;
    enable32Bit = true;
  };

  services.kanata = {
    enable = true;
    keyboards.default = {
      extraDefCfg = "process-unmapped-keys yes";
      config = ''
(defsrc
  caps
)

(defalias
  cec (tap-hold-press 200 200 esc lctl)
)

(deflayer default
  @cec
)
      '';
    };
  };

  services.mullvad-vpn.enable = true;
  services.mullvad-vpn.package = pkgs.mullvad-vpn;

  services.synergy.server = {
    enable = true;
    autoStart = true;
    screenName = "chipb0x";
    configFile = pkgs.writeText "synergy.conf" ''
      section: screens
        chipb0x:
        MacBook-Pro-M1:
      end

      section: links
        chipb0x:
          down = MacBook-Pro-M1
        MacBook-Pro-M1:
          up = chipb0x
      end
    '';
  };

  programs.gnupg.agent = {
    enable = true;
    pinentryPackage = pkgs.pinentry-gtk2;
    enableSSHSupport = true;
  };
}
