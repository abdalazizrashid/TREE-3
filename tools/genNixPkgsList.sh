#! /usr/bin/env bash
packages=$(nix-env -qa -P | awk '{print $1}')
# for line in $packages; do
         
# 	 echo -e $line\t \"$(nix-env -qa --description -A $line)\" >> packages.txt;
# done

echo "$packages" | xargs -n 1 -P ${nproc} -I {} bash -c \
  'echo -e {} "\t \"$(nix-env -qa --description -A {})\"" >> packages.txt'
