{ config, lib, ... }:
{
  networking = {
    useDHCP = false;

    # No local firewall.
     nat.enable = false;
     firewall.enable = lib.mkForce false;
     firewall.allowedTCPPorts = [ 22 ]; # TODO: open this on ygg only 
    nftables = {
      enable = true;
      ruleset = ''
        table inet filter {
          chain input {
            type filter hook input priority 0; policy drop;

            iifname { "br-lan" } counter accept comment "Allow local network to access the router"
            iifname "enp6s0" ct state { established, related } counter accept comment "Allow established traffic"
            iifname "enp6s0" icmp type { echo-request, destination-unreachable, time-exceeded } counter accept comment "Allow select ICMP"
            iifname "enp6s0" counter drop comment "Drop all other unsolicited traffic from enp6s0"
            iifname "lo" accept comment "Accept everything from loopback interface"
          }
          chain forward {
            type filter hook forward priority filter; policy drop;

            iifname { "br-lan" } oifname { "enp6s0" } counter accept comment "Allow trusted LAN to ENP6S0"
            iifname { "enp6s0" } oifname { "br-lan" } ct state { established, related } counter accept comment "Allow established back to LANs"
          }
        }

        table ip nat {
          chain postrouting {
            type nat hook postrouting priority 10; policy accept;
            oifname "enp6s0" counter masquerade
          }
        }
      '';
    };
  };
}
