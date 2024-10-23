{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.T.desktop;
  # Set your low battery threshold (percentage)
  LowBatteryThreshold = 13;
in
{
  options = {
    T.desktop.enable = lib.mkEnableOption "Enable sway desktop";
  };
  config = lib.mkIf cfg.enable {
    environment.systemPackages = with pkgs; [
      libnotify
    ];
    
      systemd.user.services."low-battery-notifier" = {
        script = ''
          set -eu
          # Get the battery percentage using upower or acpi (choose one based on your system)
          BATTERY_PERCENTAGE=$(cat /sys/class/power_supply/macsmc-battery/capacity)

          # Alternatively, if using acpi:
          # BATTERY_PERCENTAGE=$(acpi -b | grep -P -o '[0-9]+(?=%)')

          # Check if battery percentage is below the threshold
          if [[ $BATTERY_PERCENTAGE -lt ${toString LowBatteryThreshold} ]]; then
            ${pkgs.libnotify}/bin/notify-send "Low Battery" "⚠️ Battery level is at ''${BATTERY_PERCENTAGE}%"
          fi

        '';
        serviceConfig = {
          Type = "oneshot";
        };
        startAt = "*:0/5";
      };
    
  };
}
