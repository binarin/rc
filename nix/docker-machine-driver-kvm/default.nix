{stdenv, fetchurl, patchelf, libvirt, ...}:

stdenv.mkDerivation rec {
  name = "docker-machine-driver-kvm-${version}";
  version = "0.7.0";
  src = fetchurl {
    url = "https://github.com/dhiltgen/docker-machine-kvm/releases/download/v${version}/docker-machine-driver-kvm";
    sha256 = "0cashix9k32695jjz3j0751dby5adas8zsrgji0ssai3bgmkxzy4";
  };

  rpath = stdenv.lib.makeLibraryPath [libvirt];

  builder = builtins.toFile "builder.sh" ''
    set -x
    . $stdenv/setup
    mkdir -p $out/bin
    exe=$out/bin/docker-machine-driver-kvm
    cp $src $exe
    chmod 0755 $exe
    patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" $exe
    patchelf --set-rpath $rpath $exe
  '';
}
