{
  config,
  pkgs,
  lib,
  inputs,
  ...
}:
let
  root = ./.;
  nameToConfig = name: root + "/${name}/configuration.nix";
  readDirSteps = [
    builtins.readDir
    (lib.filterAttrs (name: type: type == "directory" && builtins.pathExists (nameToConfig name)))
  ];
  dirToHostConfig = [
    (lib.attrNames)
    (lib.concatMap (
      name:
      let
        p = (root + "/${name}/host-config.nix");
      in
      lib.optionals (builtins.pathExists p) [ p ]
    ))
  ];
  dirToVM = [
    (lib.mapAttrs (name: _: nameToConfig name))
    (
      let
        foldlAttrs =
          f: x0: attrs:
          builtins.foldl' (acc: name: f acc name attrs.${name}) x0 (builtins.attrNames attrs);
      in
      foldlAttrs
        (
          { n, result }:
          vm-name: path: {
            n = n + 1;
            result = result // {
              "${vm-name}" = {
                inherit pkgs;
                specialArgs.host-config = config;
                config = {
                  # microvm.hypervisor = lib.mkDefault "cloud-hypervisor";
                  microvm.shares = [
                    {
                      source = "/nix/store";
                      mountPoint = "/nix/.ro-store";
                      tag = "ro-store";
                      # proto = "virtiofs";
                      proto = "9p";
                    }
                  ];
                  microvm.writableStoreOverlay = "/nix/.rw-store";
                  microvm.interfaces = [
                    {
                      type = "tap";
                      id = "p-uvm-${builtins.substring 0 4 vm-name}";
                      mac = "02:00:00:01:33:0${toString n}"; # FIXME: handle n>9
                    }
                  ];

                  systemd.network.enable = true;
                  systemd.network.networks."20-lan" = {
                    matchConfig.Type = "ether";
                    networkConfig = {
                      IPv6AcceptRA = true;
                      DHCP = "yes";
                    };
                  };

                  imports = [ path ];
                  networking.hostName = lib.mkDefault vm-name;
                };
              };
            };
          }
        )
        {
          n = 0;
          result = { };
        }
    )
    ({ result, ... }: result)
    # (lib.mapAttrs (name: path: { }))
  ];
in
{
  # microvm.host.cloud-hypervisor.enable = true;
  imports = [
    (inputs."microvm.nix" + "/nixos-modules/host")
  ] ++ lib.pipe root (readDirSteps ++ dirToHostConfig);
  microvm.vms = lib.pipe root (readDirSteps ++ dirToVM);
  networking.nat = {
    internalInterfaces = [ "p-uvm*" ];
  };
  networking.firewall.extraInputRules = ''
    iifname uvm oifname wan0 accept;
  '';
  networking.firewall.allowedUDPPorts = [
    67 # dhcp
    68 # dhcp
  ];
  # Based on 80-vm-vt.network
  systemd.network.networks."20-p-uvm-taps" = {
    matchConfig.Name = "p-uvm-*";
    networkConfig = {
      Address = "0.0.0.0/28"; # Allocates a host ip at random
      DHCPServer = true;
      EmitLLDP = "customer-bridge";
      IPMasquerade = "ipv4";
      IPv6SendRA = true;
      IPv6AcceptRA = false;
      LinkLocalAddressing = true;
      LLDP = true;
    };
    dhcpServerConfig = { };
    linkConfig.RequiredForOnline = "no";
  };
  systemd.network.wait-online.ignoredInterfaces = [ "p-uvm-+" ];
}
