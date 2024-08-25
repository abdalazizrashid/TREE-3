{ config, ... }:

let
  probRuDC = "89.208.79.106";
in
{
  systemd.network.networks.jailite21 = {
    routes = [
      { Destination = "${probRuDC}/32"; } # probabilistic.ru datacenter l2tp entrypoint
    ];
  };
  # systemd.services."microvm@prob-ru-l2tp".after = [ "sops-nix.service" ];
  # systemd.services."microvm@prob-ru-l2tp".requires = [ "sops-nix.service" ];
  sops.secrets."probRu/dc/l2tp/user" = {
    owner = "systemd-network";
    sopsFile = ../../secrets/l2tp-dc.yaml;
    format = "yaml";
  };
  sops.secrets."probRu/dc/l2tp/psk" = {
    owner = "systemd-network";
    sopsFile = ../../secrets/l2tp-dc.yaml;
    format = "yaml";
  };
  sops.secrets."probRu/dc/ipsec" = {
    owner = "systemd-network";
    sopsFile = ../../secrets/l2tp-dc.yaml;
    format = "yaml";
  };

  sops.templates."probRu/ipsec.secrets".content = ''
    10.23.4.1 : PSK "${config.sops.placeholder."probRu/dc/ipsec"}"
  '';
  # Copy-paste from https://libreswan.org/wiki/VPN_server_for_remote_clients_using_IKEv1_with_L2TP#L2TP/IPsec_client_configuration
  sops.templates."probRu/xl2tpd.conf".content = ''
    ; Connect as a client to a server at ...
    [lac L2TPserver]
    lns = ${probRuDC}
    require chap = yes
    refuse pap = yes
    require authentication = yes
    name = ${config.sops.placeholder."probRu/dc/l2tp/user"}
    ppp debug = yes
    pppoptfile = ${config.sops.templates."probRu/options.l2tpd.client".path}
    length bit = yes
  '';
  sops.templates."probRu/options.l2tpd.client".content = ''
    ipcp-accept-local
    ipcp-accept-remote
    refuse-eap
    require-mschap-v2
    noccp
    noauth
    idle 1800
    mtu 1200
    mru 1200
    defaultroute
    noipdefault
    usepeerdns
    debug
    lock
    connect-delay 5000
    name ${config.sops.placeholder."probRu/dc/l2tp/user"}
    password ${config.sops.placeholder."probRu/dc/l2tp/psk"}
  '';
  # services.strongswan-swanctl = {
  #   enable = true;
  #   includes = [ config.sops.templates."swanctl.secrets".path ];
  #   swanctl.connections.probRu = {
  #     local_addrs = [ "10.23.4.136" ];
  #     remote_addrs = [
  #       "10.23.4.1"
  #       "10.23.4.14"
  #     ];
  #     local.probRu.auth = "psk";
  #     remote.probRu.auth = "psk";
  #     children.probRu = {
  #       remote_ts = [ "10.23.4.0/24" ];
  #     };
  #   };
  # };
  # systemd.network.netdevs."20-probRu" = {
  #     Kind = "vti"; # l2tp/ipsec?
  #     Name = "probRu";
  #     tunnelConfig = {
  #       Remote = "89.208.79.106";
  #     };
  # };
}
