{ pkgs, ... }:

{
  services.jellyfin.enable = true;

  environment.systemPackages = with pkgs; [
    jellyfin
    jellyfin-web
    jellyfin-ffmpeg
  ];

  networking.firewall.interfaces."eno1".allowedTCPPorts = [ 8096 ];
}
