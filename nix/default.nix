{ system ? builtins.currentSystem }:
let
  pkgs = import <nixpkgs> { inherit system; };
in rec {
  myR = pkgs.rWrapper.override { packages = with pkgs.rPackages; [ plyr getopt proto ggplot2 ]; };
  insync = pkgs.callPackage ./insync {
    version = "1.3.6.36076";
    sha_32 = "06fwi3cyqvkgglwb58kx7l66h0v4bjp9hxy8rg6pz7rvwi1bb1gd";
    sha_64 = "1yya4yr6p9m6cjcak54hs2ldxnp9vsfmx4wpjlf3wxanjm1qs19p";
  };
  docker-machine-driver-kvm = pkgs.callPackage ./docker-machine-driver-kvm {};
}
