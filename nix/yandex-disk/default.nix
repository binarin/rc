{stdenv, dpkg, fetchurl, makeWrapper, patchelf, zlib, gcc}:

let
  version = "0.1.5.948";
  sha256 = {
    "x86_64-linux" = "13xrvkfwjh2dpgf8639p5fiz4w4in03z32r3gn7rb7xpzsdjkpzz";
    "i686-linux" = "17pvf22w0c4ck4kfqvfxbj2p9spwwm5g4ypjrfmf19cmxlhrsx82";
  }."${stdenv.system}" or (throw "system ${stdenv.system} not supported");

  arch = {
    "x86_64-linux" = "amd64";
    "i686-linux" = "i386";
  }."${stdenv.system}" or (throw "system ${stdenv.system} not supported");

  libPath = stdenv.lib.makeLibraryPath [
    zlib gcc.cc
  ];

in stdenv.mkDerivation {
  name = "yandex-disk";
  src = fetchurl {
    name = "yandex-disk-${version}.deb";
    url = "http://repo.yandex.ru/yandex-disk/yandex-disk_latest_${arch}.deb";
    inherit sha256;
  };

  buildInputs = [ dpkg patchelf ];

  phases = [ "unpackPhase" "installPhase" "fixupPhase" ];
  unpackPhase = ''
    dpkg-deb -x $src yandex-disk
  '';
  installPhase = ''
    mkdir -p $out
    cp -r yandex-disk/usr/bin $out
    cp -r yandex-disk/usr/share $out
  '';

  fixupPhase = ''
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      --set-rpath "${libPath}" \
      "$out/bin/yandex-disk"
  '';

  meta = {
    homepage = "https://disk.yandex.com/";
    description = "Online stored folders (daemon version)";
    platforms = [ "i686-linux" "x86_64-linux" ];
  };
}
