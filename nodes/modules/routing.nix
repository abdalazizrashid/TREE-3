{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.T.routing;
in
{
  options = {
    T.routing.enable = lib.mkEnableOption "Enable the router on this node";
  };
  config = lib.mkIf cfg.enable {
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
            # IPv6PrivacyExtensions = true;
            IPv4Forwarding = true;
            IPv6Forwarding = true;
          };
          # make routing on this interface a dependency for network-online.target
          linkConfig.RequiredForOnline = "routable";
        };
      };
    };
  };
}
