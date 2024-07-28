
{config, lib, pkgs, ...}:
let
  cfg = config.services.knot;
  knot.enable = true;
  zonesDir = "/var/lib/knot/zones/";
  zones."aziz.fyi" = pkgs.writeTextDir "aziz.fyi.zone"
  zones."hosts.aziz.fyi" = pkgs.writeTextDir  "hosts.aziz.fyi.zone" 
  knotZonesEnv = pkgs.buildEnv {
    name = "knot-zones";
    paths = [ zones."aziz.fyi" zones."hosts.aziz.fyi" ];
  };
  # DO NOT USE pkgs.writeText IN PRODUCTION. This put secrets in the nix store!
  tsigFile = pkgs.writeText "tsig.conf" ''
#### hmac-sha256:aziz.fyi:TCf2iOE7L/W8ekptXKyAYVMz8mIzFSoB3d4Y7SoRA+g=
key:
  - id: aziz.fyi
    algorithm: hmac-sha256
    secret: TCf2iOE7L/W8ekptXKyAYVMz8mIzFSoB3d4Y7SoRA+g=
  '';
in
{
  # systemd.tmpfiles.rules = [
  #   "R ${zonesDir}   -     -      -   -" # Remove old zones
  #   "C+ ${zonesDir} - - - - ${../../infra/zones}" # Copy the zones file
  #   "z ${zonesDir} 0644 knot knot - -"  # Change permissions
  # ];
      
  services.knot = {
    enable = true;
    extraArgs = [ "-v" ];
    keyFiles = [ tsigFile ];
    settingsFile = pkgs.writeText "knot.conf" ''
      mod-rrl:
        - id: default
          rate-limit: 200   # Allow 200 resp/s for each flow
          slip: 2           # Every other response slips

      policy:
        - id: ececc
          algorithm: ecdsap384sha384
          nsec3: on
        - id: rsa
          algorithm: RSASHA256
          ksk-size: 2048
          zsk-size: 1024

      template:
          - id: default
            storage: ${knotZonesEnv}
            semantic-checks: on
            dnssec-signing: on
            dnssec-policy: ececc
            global-module: mod-rrl/default   # Enable RRL globally
            # Input-only zone files
            # https://www.knot-dns.cz/docs/2.8/html/operation.html#example-3
            # prevents modification of the zonefiles, since the zonefiles are immutable
            zonefile-sync: -1
            zonefile-load: difference
            journal-content: changes

      server:
          listen: ::@53
          listen: 0.0.0.0@53

      acl:
        - id: update_acl
          address: 127.0.0.1
          action: update

      zone:
        - domain: aziz.fyi
          file: aziz.fyi.zone

        - domain: hosts.aziz.fyi
          file: hosts.aziz.fyi.zone
          acl: update_acl

      log:
        - target: syslog
          any: debug
    '';
  };
}
# {
 
#   services.bind = {
#     enable = true;
#     zones = [aziz hosts];
#     extraConfig = ''
#       logging {
#           channel query_log {
#               file "/var/log/bind/query.log" versions 3 size 10m;
#               severity info;
#               print-time yes;
#           };

#           category queries { query_log; };
#       };
#     '';
#     extraOptions = " ";
#   };
#   # cf. for more details on systemd tempfiles rules
#   # https://www.freedesktop.org/software/systemd/man/latest/tmpfiles.d.html
#   systemd.tmpfiles.rules = [
#     "d /var/log/bind 0750 named named -"
#     "f /var/log/bind/query.log 0640 named named -"
#     "R ${zonesDir}   -     -      -   -" # Remove old zones
#     "d ${zonesDir} 0750 named named -"
#     "C+ ${zonesDir}${hosts.name} - - - - ${hostsZone}"
#     "z ${hostsZone} 0644 named named - -"
#   ];
