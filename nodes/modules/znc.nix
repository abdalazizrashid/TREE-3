{
  config,
  pkgs,
  lib,
  ...
}:
{
  services.znc = {
    enable = true;
    openFirewall = true;
    extraFlags = [ "--debug" ];
    confOptions = {
      nick = "afdee1c";
      port = 5000;
      useSSL = true;
      userName = "znc";
      passBlock = ''
<Pass password>
	        Method = sha256
	        Hash = 20dc06f6c8be3a96e3494865e6d343fa543951174c8c303211a0f7587fd2c845
	        Salt = K6(Y)3uHhxvf/K-T.0la
</Pass>
      '';
      modules = [ "webadmin" "adminlog" "log" "partyline" ];
      
    };
    confOptions.networks = {
      "libera" = {
        server = "irc.libera.chat";
        port = 6697;
        useSSL = true;
        modules = [ "simple_away" ];
        channels = [
          "nixos"
          "emacs"
          "git"
          "math"
          "physics"
          "Llamas"
          "regex"
        ];
      };
      "gmpnet" = {
        server = "irc.gimp.net";
        port = 6697;
        useSSL = true;
        modules = [ "simple_away" ];
        channels = [
          "gnucash"
        ];
      };
      "hackinit" = {
        server = "irc.hackinit.org";
        port = 6697;
        useSSL = true;
        modules = [ "simple_away" ];
        channels = [
          "tvl"
        ];
      };

    };
  };
}
