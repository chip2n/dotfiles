{ config, pkgs, ... }:

{
  home.username = "chip";
  home.homeDirectory = "/home/chip";
  home.sessionPath = [ "$HOME/.local/bin" ];

  home.packages = with pkgs; [
    direnv
    firefox
    ripgrep
    synergy
  ];

  programs.bash = {
    enable = true;
  };

  programs.git = {
    enable = true;
    userEmail = "andreas@arvidsson.io";
    userName = "chip2n";
  };

  xdg.configFile.emacs.source = config.lib.file.mkOutOfStoreSymlink /home/chip/dev/dotfiles/.emacs.d;

  home.stateVersion = "24.05";

  # Let home manager install and manage itself
  programs.home-manager.enable = true;
}
