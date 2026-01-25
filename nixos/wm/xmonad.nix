{ config, pkgs, ... }:

{
  services.xserver.desktopManager.wallpaper = {
    combineScreens = false;
    mode = "scale";
  };
  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
    config = builtins.readFile ../../.config/xmonad/xmonad.hs;
  };
}
