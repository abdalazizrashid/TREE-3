{config, lib, pkgs, ...}:
{
    # services.headscale = {
    #   enable = true;
    #   address = "0.0.0.0";
    #   port = 8080;
    #   settings.server_url = "https://aziz.fyi";
    #   settings.dns = { baseDomain = "example.com"; };
    #   settings = { logtail.enabled = false; };
    # };

    services.nginx.virtualHosts."aziz.fyi" = {
      forceSSL = true;
      enableACME = true;
      locations."/" = {
        proxyPass =
          "http://localhost:${toString config.services.headscale.port}";
          proxyWebsockets = true;
      };
    };
}
