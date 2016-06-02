{stdenv, fetchurl, patchelf, version, sha_32, sha_64}:

let
  arch = {
    "x86_64-linux" = "amd64";
    "i686-linux" = "i386";
  }."${stdenv.system}" or (throw "system ${stdenv.system} not supported");
  sha256 = {
    "x86_64-linux" = sha_64;
    "i686-linux" = sha_32;
  }."${stdenv.system}" or (throw "system ${stdenv.system} not supported");
in
stdenv.mkDerivation {
  name = "insync";
  src = fetchurl {
    name = "insync-portable_${version}_${arch}.tar.bz2";
    url = "http://s.insynchq.com/builds/insync-portable_${version}_${arch}.tar.bz2";
    inherit sha256;
  };

  script = builtins.toFile "insync" ''
    #!/usr/bin/env bash
    cd $(readlink -f $(dirname $0))/../client
    LC_TIME=C exec ./insync-portable "$@"
  '';

  buildInputs = [patchelf];

  installPhase = ''
    mkdir -p $out/bin
    cp -r client $out/client
    cp $script $out/bin/insync
    chmod +x $out/bin/insync
  '';

  fixupPhase = ''
    patchelf \
      --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" \
      "$out/client/insync-portable"
  '';
  meta = {
    homepage = "https://www.insynchq.com/";
    description = "Google Drive client";
    platforms = [ "i686-linux" "x86_64-linux" ];
  };
}
