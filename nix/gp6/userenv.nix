{pkgs, stdenv, bash, buildFHSUserEnv, writeScript, ...}:

buildFHSUserEnv {
  name = "guitar-pro";
  targetPkgs = pkgs: (with pkgs; [
    (pkgs.callPackage ./ssl.0.9.8.nix {})
    (writeScriptBin "gksudo" ''
      #!${bash}/bin/bash
      set -euxo pipefail
      shift
      shift
      exec "$@"
    '')
    liberation_ttf
    procps
    strace
    # sudo
    which
    alsaLib
    mesa
    portaudio
    stdenv.cc.cc
    libxml2
    libxslt
    zlib
    libpulseaudio
    xorg.libX11
    xorg.libSM
    xorg.libICE
    xorg.libXrender
    xorg.libXext
    fontconfig
    freetype
    glib
    libvorbis
    libpng12
  ]);
  runScript = writeScript "launch-gp6-in-userenv" ''
    #!${bash}/bin/bash
    set -euxo pipefail
    if [[ ! -d $HOME/GuitarPro6/opt/GuitarPro6 ]]; then
      echo There is no Guitar Pro pre-unpacked in user home
      exit 1
    fi
    GP=$HOME/GuitarPro6/opt/GuitarPro6
    cd $GP
    # export LD_LIBRARY_PATH=$(pwd)''${LD_LIBRARY_PATH:+:}$LD_LIBRARY_PATH
    # export LANG=en_US.UTF-8
    # I wasn't able to trace why updater fails to work under GuitarPro itself.
    # Because under 'strace' it works flawlesly
    # And error message is garbled in error dialog box
    # So there is a separate cmdline to run updater manually
    # exec strace -ff -o $HOME/tmp/gp/1.log -s 0 ./GuitarPro
    if [[ "$#" -ne 0 ]]; then
      case "$1" in
        update)
          $GP/GPUpdater $GP
        ;;
        bash)
          exec ${bash}/bin/bash -i
        ;;
        strace)
          shift
          strace "$@"
        ;;
        *)
          echo "Unknown command: $1"
          exit 1
        ;;
      esac
    else
      ./GuitarPro
    fi
  '';
}
