{config, lib, pkgs, ...}:
{
  security.acme = {
      acceptTerms = true;
      defaults.email = "admin+acme@aziz.fyi";
      certs."aziz.fyi" = {
        dnsProvider = "porkbun";
        # Supplying password files like this will make your credentials world-readable
        # in the Nix store. This is for demonstration purpose only, do not use this in production.
        environmentFile = "${pkgs.writeText "porkbun-creds" ''
          PORKBUN_API_KEY=pk1_3b9079d93e0ad9fb680c080bd7ea5060ee37efc79f1d9436692d57d38716b01b
          PORKBUN_SECRET_API_KEY=sk1_1449afeb820f6817c91d39282d4dca373b2bf3bbfa7ba874b2fd5ead1d474635
        ''}";
      };
  };
}
