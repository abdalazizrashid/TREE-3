{ pkgs, ... }:

let
  env = builtins.toFile "env.js" ''
    window.env = {
      SERVER_URL: "95.165.26.135:4747",
    }
  '';

  airsonicDist = pkgs.fetchzip {
    name = "airsonic-refix";

    # from master CI @ f894d5eacebec2f47486f340c8610f446d4f64b3
    # https://github.com/tamland/airsonic-refix/actions/runs/6150155527
    url = "https://storage.yandexcloud.net/tazjin-public/airsonic-refix-f894d5ea.zip";
    sha256 = "02rnh9h7rh22wkghays389yddwbwg7sawmczdxdmjrcnkc7mq2jz";

    stripRoot = false;
    postFetch = "cp ${env} $out/env.js";
  };
in
{
#   services.nginx.virtualHosts."player.tazj.in" = {
#     enableACME = true;
#     forceSSL = true;
#     root = "${airsonicDist}";

#     # deal with SPA routing requirements
#     locations."/".extraConfig = "try_files $uri /index.html;";
#   };
# }
