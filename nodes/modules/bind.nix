{config, lib, pkgs, ...}:
let
  cfg = config.services.bind;
  # TODO consider using DNS nix dsl
  # https://github.com/nix-community/dns.nix
  zonesDir = "/var/lib/bind/";
 
  checkZone = zone: file: pkgs.runCommand "${zone}-check" { }
    ''
      ${pkgs.bind}/bin/named-checkzone -i local ${zone} ${file} | tee $out
    '';
    
  aziz = {
    name = "aziz.fyi";
    master = true;
    file = checkZone aziz.name ../../infra/zones/aziz.fyi.zone;
    masters = [ ];
    slaves = [ ];
    extraConfig = "";
  };
  
  hosts = {
    name = "hosts.aziz.fyi";
    master = true;
    file = "${zonesDir}hosts.aziz.fyi";
    masters = [ ];
    slaves = [ ];
    extraConfig = ''
      allow-update { 127.0.0.1; };
    '';
  };
  hostsZone = pkgs.writeText hosts.name ''
    $TTL 86400
    @       IN      SOA     ns1.hosts.aziz.fyi. admin.hosts.aziz.fyi. (
                                2024072201 ; Serial
                                3600       ; Refresh
                                1800       ; Retry
                                1209600    ; Expire
                                86400 )    ; Minimum TTL

            IN      NS      ns1.hosts.aziz.fyi.
            IN      NS      ns2.hosts.aziz.fyi.

    ns1     IN      A       127.0.0.1
    ns2     IN      A       127.0.0.1

    @       IN      A       203.0.113.10
    www     IN      A       203.0.113.10
    t1      IN      A       95.165.26.135
  '';

in
{
 
  services.bind = {
    enable = true;
    zones = [aziz hosts];
    extraConfig = ''
      logging {
          channel query_log {
              file "/var/log/bind/query.log" versions 3 size 10m;
              severity info;
              print-time yes;
          };

          category queries { query_log; };
      };
    '';
    extraOptions = " ";
  };
  # cf. for more details on systemd tempfiles rules
  # https://www.freedesktop.org/software/systemd/man/latest/tmpfiles.d.html
  systemd.tmpfiles.rules = [
    "d /var/log/bind 0750 named named -"
    "f /var/log/bind/query.log 0640 named named -"
    "R ${zonesDir}   -     -      -   -" # Remove old zones
    "d ${zonesDir} 0750 named named -"
    "C+ ${zonesDir}${hosts.name} - - - - ${hostsZone}"
    "z ${hostsZone} 0644 named named - -"
  ];

  environment.systemPackages = [
    pkgs.bind
  ];
}
# aziz.fyi.		600	IN	A	185.199.110.153
# aziz.fyi.		600	IN	A	185.199.108.153
# aziz.fyi.		600	IN	A	185.199.109.153
# aziz.fyi.		600	IN	A	185.199.111.153
# A	t1.aziz.fyi	95.165.26.135	600		
 
# notes	Thinkpad
# AAAA	aziz.fyi	2606:50c0:8000::153	600		
 
# AAAA	aziz.fyi	2606:50c0:8001::153	600		
 
# AAAA	aziz.fyi	2606:50c0:8002::153	600		
 
# AAAA	aziz.fyi	2606:50c0:8003::153	600		
 
# CNAME	special.aziz.fyi	pixie.porkbun.com	600		
 
# CNAME	stats.aziz.fyi	aziz.goatcounter.com.	600		
 
# CNAME	sig1._domainkey.aziz.fyi	sig1.dkim.aziz.fyi.at.icloudmailadmin.com.	600		
 
# MX	aziz.fyi	mx01.mail.icloud.com.	600	10	
 
# MX	aziz.fyi	mx02.mail.icloud.com.	600	10	
 
# TXT	aziz.fyi	apple-domain=ScWBcm5aHDykHIeV	600		
 
# TXT	aziz.fyi	"v=spf1 include:icloud.com ~all"	600		
 
# TXT	_github-pages-challenge-abdalazizrashid.aziz.fyi	1bff295337e8c74175c6a32014bcd9	600
