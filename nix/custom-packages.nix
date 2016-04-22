{ system ? builtins.currentSystem }:
let
  pkgs = import <nixpkgs> { inherit system; };
in rec {
  yandex-disk = pkgs.callPackage ./yandex-disk {};
  erlang18 = pkgs.callPackage ./erlang/R18.nix {
    Carbon = null;
    Cocoa = null;
    wxSupport = true;
    javacSupport = true;
    enableHipe = true;
  };
}
