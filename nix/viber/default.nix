with import <nixpkgs> {}; {
  config.allowUnfree = true;
  viberEnv = stdenv.mkDerivation {
    name = "viberr";
    buildInputs = [
      (stdenv.lib.overrideDerivation viber (oldAttrs:
        { src = fetchurl {
              url = "http://download.cdn.viber.com/cdn/desktop/Linux/viber.deb";
              sha256 = "026vp2pv66b2dlwi5w5wk4yjnnmnsqapdww98p7xdnz8n0hnsbbi";
           };
        }))
   ];
 };
}
