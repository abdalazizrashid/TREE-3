{ config, pkgs, ... }:
{
  systemd.network.enable = true;
  systemd.network.netdevs."20-blackhole-wg" = {
    enable = true;
    netdevConfig = {
      Kind = "wireguard";
      Name = "wgbh0";
      MTUBytes = "1400";
    };
    wireguardConfig = {
      # TODO: migrate to sops-nix; owned by systemd-network
      PrivateKeyFile = "/var/lib/wg/blackhole";
    };
    wireguardPeers = [
      {
        wireguardPeerConfig = {
          PersistentKeepalive = 24;
          PublicKey = "HOJKD/tYdvCS5ShUX+D0Sdwe4bFuwlduo2oyF4s0CSM=";
          # Endpoint = "51.250.15.95:51000";
          Endpoint = "89.208.79.106:51000"; # Default: 51820
          AllowedIPs = [
            "10.10.9.0/24"
            "10.231.1.0/24"
            "10.140.0.0/24"
	    "10.23.4.0/24"
          ];
        };
      }
    ];
  };
  systemd.network.networks."20-blackhole-wg" = {
    matchConfig.Name = "wgbh0";
    address = [
      # Chosen arbitrarily
      "10.23.4.69/32"
      "10.10.9.69/24"
      "10.231.1.69/30"
      "10.140.0.69/30"
    ];
    DHCP = "yes";

    # Gateway is maybe 10.10.9.1/31?
    routes = [
      {
        routeConfig = {
          Destination = "10.10.9.0/24";
          Gateway = "10.10.9.1";
          PreferredSource = "10.10.9.69";
        };
      }
      {
        routeConfig = {
          Destination = "10.231.1.0/24";
          Gateway = "10.10.9.1";
        };
      }
      {
        routeConfig = {
          Destination = "10.140.0.0/24";
          Gateway = "10.10.9.1";
        };
      }
      {
        routeConfig = {
          Destination = "10.23.4.0/24";
          Gateway = "10.10.9.1";
        };
      }
    ];
  };
  systemd.services."systemd-networkd".environment.SYSTEMD_LOG_LEVEL = "debug";
}
