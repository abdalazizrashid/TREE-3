{ config, pkgs, ... }:
let
  a = "";
in
{
  systemd.tmpfiles.rules = [
    "d /tmp/fake-podcasts 0555 nobody nobody -"
    "d /tmp/fake-playlists 0555 nobody nobody -"
  ];

  services.gonic = {
    enable = true;
    settings = {
      listen-addr = "0.0.0.0:4747";
      scan-interval = 5;
      scan-at-start-enabled = true;
      podcast-path = [ "/tmp/fake-podcasts" ];
      playlists-path = [ "/tmp/fake-playlists" ];
      music-path = [ "/var/lib/music" ];
    };
  };

  # hack to work around the strict sandboxing of the gonic module
  # breaking DNS resolution
  systemd.services.gonic.serviceConfig.BindReadOnlyPaths = [
    "-/etc/resolv.conf"
  ];

  services.airsonic = {
    enable = true;
    
  };
  # add a hard dependency on the FUSE mount
#  systemd.services.gonic.requires = [ "geesefs.service" ];

#   services.nginx.virtualHosts."music.tazj.in" = {
#     addSSL = true;
#     enableACME = true;

#     locations."/" = {
#       proxyPass = "http://127.0.0.1:4747";
#     };
#   };
 }
