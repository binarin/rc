with import <nixpkgs> {}; {
  vagrantEnv = stdenv.mkDerivation {
    name = "vagrant-with-libvirt-deps";
    buildInputs = [ libvirt libxml2 libxslt pkgconfig ];
    LD_LIBRARY_PATH = stdenv.lib.makeLibraryPath [ libvirt libxml2 libxslt ];
    shellHook = ''
      for vagrantPluginToInstall in vagrant-libvirt vagrant-vbguest; do
        if ! vagrant plugin list | grep -q $vagrantPluginToInstall ; then
          vagrant plugin install $vagrantPluginToInstall
        fi
      done
    '';
  };
}
