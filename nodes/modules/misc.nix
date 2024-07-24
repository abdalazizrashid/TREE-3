{config, lib, pkgs, ...}:
{
    environment.etc."nextcloud-admin-pass".text = "allyouneedislove";
    services.nextcloud = {
      enable = true;
      package = pkgs.nextcloud28;
      hostName = "localhost";
      config.adminpassFile = "/etc/nextcloud-admin-pass";
    };

    services.calibre-server = {
      enable = true;
    };
    services.netbird.server = {
      enable = false;

      domain = "net.aziz.fyi";

      enableNginx = true;

      coturn = {
        enable = false;

        passwordFile = "/etc/netbird-pswd";
      };

      management = {
        #oidcConfigEndpoint = "https://sso.example.selfhosted/oauth2/openid/netbird/.well-known/openid-configuration";

        settings = {
          TURNConfig = {
            Turns = [
              {
                Proto = "udp";
                URI = "turn:net.aziz.fyi:3478";
                Username = "netbird";
                Password._secret = "/etc/netbird-pswd";
              }
            ];
          };
        };
      };
    };

}
