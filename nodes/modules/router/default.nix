{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.T.router;
in
{
  imports = [
    ./routing.nix
  ];
  options = {
    T.router.enable = lib.mkEnableOption "Enable the router on this node";
    #T.verboseLog = lib.mk
  };
  config = lib.mkIf cfg.enable {
    systemd.managerEnvironment = {
      SYSTEMD_LOG_LEVEL = "debug";
    };

    networking.firewall.enable = true;
    networking.firewall.allowedTCPPorts = [ 22 ];

    networking.useNetworkd = true;
    systemd.network = {
      wait-online.anyInterface = true;

      # add bridge to bridge all the lan interfaces
      netdevs = {
        "20-br-lan" = {
          netdevConfig = {
            Kind = "bridge";
            Name = "br-lan";
          };
        };
      };

      networks = {
        "40-br-lan" = {
          matchConfig.Name = "br-lan";
          bridgeConfig = { };
          address = [
            "192.168.10.1/24"
          ];
          networkConfig = {
            ConfigureWithoutCarrier = true;
          };
        };

        "30-enp4s0" = {
          matchConfig.Name = "enp4s0";
          linkConfig.RequiredForOnline = "enslaved";
          networkConfig = {
            Bridge = "br-lan";
            ConfigureWithoutCarrier = true;
          };
        };

        "10-wan" = {
          matchConfig.Name = "enp6s0";
          networkConfig = {
            # start a DHCP Client for IPv4 Addressing/Routing
            DHCP = "ipv4";
            DNSOverTLS = true;
            DNSSEC = true;
            IPv6PrivacyExtensions = true;
            IPv4Forwarding = true;
            IPv6Forwarding = true;
          };
          # make routing on this interface a dependency for network-online.target
          linkConfig.RequiredForOnline = "routable";
        };
      };
    };
    services.kea = {
      dhcp4 = {
        enable = false;
        settings = {
          interfaces-config = {
            interfaces = [
              "br-lan"
            ];
          };
          lease-database = {
            name = "/var/lib/kea/dhcp4.leases";
            persist = true;
            type = "memfile";
          };
          rebind-timer = 2000;
          renew-timer = 1000;
          subnet4 = [
            {
              id = 1;
              pools = [
                {
                  pool = "192.168.10.50 - 192.168.10.253";

                }
              ];
              subnet = "192.168.10.0/24";
            }
          ];
          valid-lifetime = 4000;
          option-data = [
            {
              name = "routers";
              data = "192.168.10.254";
            }
          ];
          loggers = [
            {
              name = "kea-dhcp4";
              output_options = [
                {
                  output = "/tmp/kea-dhcp4.log";
                  maxver = 10;
                }
              ];
              severity = "INFO";
            }
          ];
        };
      };
      dhcp6 = {
        enable = false;
        settings = {
          interfaces-config.interfaces = [ "br-lan" ];
        };
      };
      dhcp-ddns.enable = false;
    };
    services.resolved.extraConfig = ''
      DNSStubListener=no
    '';
    services.dnsmasq = {
      enable = true;
      settings = {
        # upstream DNS servers
        server = [
          "9.9.9.9"
          "8.8.8.8"
          "1.1.1.1"
        ];
        # sensible behaviours
        domain-needed = true;
        bogus-priv = true;
        no-resolv = true;

        # Cache dns queries.
        cache-size = 1000;

        dhcp-range = [ "br-lan,192.168.10.50,192.168.10.254,24h" ];
        interface = "br-lan";
        dhcp-host = "192.168.10.1";

        # local domains
        local = "/lan/";
        domain = "lan";
        expand-hosts = true;

        # don't use /etc/hosts as this would advertise d1 as localhost
        no-hosts = true;
        address = "/d1.lan/192.168.10.1";
      };
    };
  };
}
