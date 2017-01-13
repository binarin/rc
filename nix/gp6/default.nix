{pkgs, stdenv, bash, requireFile, writeScriptBin, ...}:

let
  userenv = pkgs.pkgsi686Linux.callPackage ./userenv.nix {};
  prefetchedDeb = requireFile {
     name = "gp6-full-linux-demo-r11686.deb";
     url = "https://www.guitar-pro.com/en/index.php?pg=download";
     sha256 = "11h0g6d2g2li54ia6r1jqvdyf0hvbh3239s47y41k7g59lh47lkq";
  };
in writeScriptBin "gp6" ''
  #!${bash}/bin/bash
  set -euxo pipefail
  if [[ -z $HOME ]]; then
    echo HOME should be set
    exit 1
  fi
  if [[ ! -d $HOME/GuitarPro6/opt/GuitarPro6 ]]; then
    mkdir -p $HOME/GuitarPro6
    dpkg -x ${prefetchedDeb} $HOME/GuitarPro6
  fi
  exec ${userenv}/bin/guitar-pro "$@"
''
