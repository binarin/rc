{stdenv, dpkg, requireFile, alsaLib, mesa, portaudio, libxml2, libxslt, zlib, libpulseaudio, xorg, fontconfig, freetype, glib, libvorbis, libpng12, openssl, gksu, makeWrapper, bash, writeScript}:

let
  rpath = stdenv.lib.makeLibraryPath [
    (pkgs.callPackage ./ssl.0.9.9.nix {})
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
  ];
  version = "r11686";
in
stdenv.mkDerivation {
  inherit gksu;
  name = "guitar-pro-6-${version}";
  buildInputs = [
    dpkg makeWrapper gksu
  ];
  src = requireFile {
     name = "gp6-full-linux-demo-r11686.deb";
     url = "https://www.guitar-pro.com/en/index.php?pg=download";
     sha256 = "11h0g6d2g2li54ia6r1jqvdyf0hvbh3239s47y41k7g59lh47lkq";
  };
  unpackPhase = "true";
  buildCommand = ''
    mkdir -p $out
    dpkg -x $src $out
    cp -av $out/usr/share $out
    cp -av $out/opt/GuitarPro6 $out

    # Otherwise it looks "suspicious"
    chmod -R g-w $out

    cp ${./libcrypto.so.0.9.8} $out/GuitarPro6/libcrypto.so.0.9.8
    cp ${./libssl.so.0.9.8} $out/GuitarPro6/libssl.so.0.9.8
    chmod +x $out/GuitarPro6/libcrypto.so.0.9.8 $out/GuitarPro6/libssl.so.0.9.8

    for file in $(find $out -type f \( -perm /0111 -o -name \*.so\* \) ); do
      patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$file" || true
      patchelf --set-rpath ${rpath}:$out/GuitarPro6 $file || true
    done

    mkdir $out/bin
    cat <<EOF > $out/bin/gp6
    #!${bash}/bin/bash
    set -euxo
    cd $out/GuitarPro6/
    ./GuitarPro
    EOF
    chmod +x $out/bin/gp6

    cat <<EOF > $out/bin/gksudo
    #!${bash}/bin/bash
    echo "ARGS:" "\$@"
    set -euxo pipefail
    shift
    shift
    exec "\$@"
    EOF
    chmod +x $out/bin/gksudo

    # Fix the desktop link
    substituteInPlace $out/share/applications/GuitarPro6.desktop \
      --replace /opt/GuitarPro6/launcher.sh $out/GuitarPro6/launcher.sh \
      --replace guitarpro6.png $out/share/pixmaps/guitarpro6.png

  '';
}
