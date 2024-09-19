{ lib ? pkgs.lib, pkgs ? import <nixpkgs> { }, ... }:

(pkgs.writeShellScriptBin "qemu-system-x86_64-uefi" ''
  ${lib.getExe' pkgs.qemu "qemu-system-x86_64"} \
    -bios ${pkgs.OVMF.fd}/FV/OVMF.fd \
    "$@"
'')
