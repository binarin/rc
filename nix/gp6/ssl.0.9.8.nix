{fetchurl, dpkg, stdenv, writeScript, bash, ...}:
stdenv.mkDerivation rec {
  name = "libssl0.9.8-i386-without-patchelf";
  src = fetchurl {
    name = "libssl0.9.8_i386.deb";
    url = "http://security.ubuntu.com/ubuntu/pool/universe/o/openssl098/libssl0.9.8_0.9.8o-7ubuntu3.2.14.04.1_i386.deb";
    sha256 = "10bb68qahj85vid0m1kx6c266pdbfcxxkhf6nggfnyc4nys99p52";
  };
  buildInputs = [ dpkg ];
  buildCommand = ''
    dpkg -x $src deb-unpack
    mkdir -p $out/lib
    cp deb-unpack/lib/i386-linux-gnu/lib*.so.0.9.8 $out/lib
    chmod +x $out/lib/lib*.so.0.9.8
  '';
}
