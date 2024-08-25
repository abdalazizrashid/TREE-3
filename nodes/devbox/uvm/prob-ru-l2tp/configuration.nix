{
  host-config,
  lib,
  pkgs,
  ...
}:
let
  probRuDC = "89.208.79.106";
in
{
  # microvm.hypervisor = "cloud-hypervisor";
  microvm.shares = [
    {
      source = builtins.dirOf host-config.sops.templates."probRu/xl2tpd.conf".path;
      mountPoint = builtins.dirOf host-config.sops.templates."probRu/xl2tpd.conf".path;
      tag = "secrets-probRu";
      proto = "virtiofs";
    }
  ];
  
  # NOTE: the systemd unit is called ipsec.service...
  services.libreswan.enable = true;
  # Copy-paste from https://libreswan.org/wiki/VPN_server_for_remote_clients_using_IKEv1_with_L2TP#L2TP/IPsec_client_configuration
  services.libreswan.connections.probRu = ''
    authby=secret
    pfs=no
    auto=add
    rekey=no
    left=%defaultroute
    right=${probRuDC}
    type=transport
    leftprotoport=17/1701
    rightprotoport=17/1701
    dpddelay=30
    dpdtimeout=70
    dpdaction=clear
  '';
  systemd.services.xl2tpd-client = {
    enable = true;
    wantedBy = [ "network-online.target" ];
    serviceConfig.Type = "simple";
    serviceConfig.ExecStart = "${lib.getExe pkgs.xl2tpd} -D -c ${
      host-config.sops.templates."probRu/xl2tpd.conf".path
    }";
    serviceConfig.RuntimeDirectory = "xl2tpd";
  };
  users.users.someone = {
    isNormalUser = true;
    extraGroups = [ "wheel" ];
    hashedPassword = "";
    openssh.authorizedKeys.keys =
      host-config.users.users.serge.openssh.authorizedKeys.keys
      ++ host-config.users.users.aziz.openssh.authorizedKeys.keys
      ++ [ "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMPlMI/dPhPSsDqyyoi6+ELDgPGROm8I/SUlkEB3kIBE serge@d1" ];
  };
  services.openssh.enable = true;
}
