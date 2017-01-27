with import <nixpkgs> {}; {
  vagrantEnv = stdenv.mkDerivation {
    name = "vagrant-with-libvirt-deps";
    buildInputs = [ libvirt libxml2 libxslt pkgconfig vagrant ];
    LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [ libvirt libxml2 libxslt ];
    shellHook = ''
      set -e
      for vagrantPluginToInstall in vagrant-libvirt vagrant-vbguest; do
        if ! vagrant plugin list | grep -q $vagrantPluginToInstall ; then
          vagrant plugin install $vagrantPluginToInstall
        fi
      done
      set +e
    '';
  };
}
