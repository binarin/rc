{ stdenv, fetchFromGitHub, fetchurl, perl, gnum4, ncurses, openssl
, gnused, gawk, makeWrapper, autoconf, libxml2, libxslt, fop
, Carbon, Cocoa
, odbcSupport ? false, unixODBC ? null
, wxSupport ? true, mesa ? null, wxGTK ? null, xorg ? null, wxmac ? null
, javacSupport ? false, openjdk ? null
, enableHipe ? true
}:

assert wxSupport -> (if stdenv.isDarwin
  then wxmac != null
  else mesa != null && wxGTK != null && xorg != null);

assert odbcSupport -> unixODBC != null;
assert javacSupport ->  openjdk != null;

with stdenv.lib;

stdenv.mkDerivation rec {
  name = "erlang-" + version + "${optionalString odbcSupport "-odbc"}"
  + "${optionalString javacSupport "-javac"}";
  version = "18.3";

  src = fetchFromGitHub {
    owner = "erlang";
    repo = "otp";
    rev = "OTP-18.3.4";
    sha256 = "1f8nhybzsdmjvkmkzpjj3wj9jzx8mihlvi6gfp47fxkalansz39h";
  };

  buildInputs =
    [ perl gnum4 ncurses openssl makeWrapper autoconf libxml2 libxslt fop
    ] ++ optionals wxSupport (if stdenv.isDarwin then [ wxmac ] else [ mesa wxGTK xorg.libX11 ])
      ++ optional odbcSupport unixODBC
      ++ optional javacSupport openjdk
      ++ stdenv.lib.optionals stdenv.isDarwin [ Carbon Cocoa ];

  patchPhase = ''  '';

  preConfigure = ''
    export HOME=$PWD/../
    sed -e s@/bin/pwd@pwd@g -i otp_build
    ./otp_build autoconf
    sed -i "s@/bin/rm@rm@" lib/odbc/configure erts/configure
  '';

  configureFlags= [
    "--with-ssl=${openssl}"
  ] ++ optional enableHipe "--enable-hipe"
    ++ optional wxSupport "--enable-wx"
    ++ optional odbcSupport "--with-odbc=${unixODBC}"
    ++ optional javacSupport "--with-javac"
    ++ optional stdenv.isDarwin "--enable-darwin-64bit";

  postInstall = let
    manpages = fetchurl {
      url = "http://www.erlang.org/download/otp_doc_man_${version}.tar.gz";
      sha256 = "1hpcr7a3dx2y9gnb53bvb4g6lyvbwigadl9s3f978s01x40f32wp";
          };
  in ''
    ln -s $out/lib/erlang/lib/erl_interface*/bin/erl_call $out/bin/erl_call
    tar xf "${manpages}" -C "$out/lib/erlang"
    for i in "$out"/lib/erlang/man/man[0-9]/*.[0-9]; do
      prefix="''${i%/*}"
      ensureDir "$out/share/man/''${prefix##*/}"
      ln -s "$i" "$out/share/man/''${prefix##*/}/''${i##*/}erl"
    done
  '';

  # Some erlang bin/ scripts run sed and awk
  postFixup = ''
    wrapProgram $out/lib/erlang/bin/erl --prefix PATH ":" "${gnused}/bin/"
    wrapProgram $out/lib/erlang/bin/start_erl --prefix PATH ":" "${gnused}/bin/:${gawk}/bin"
  '';

  setupHook = ./setup-hook.sh;

  meta = {
    homepage = "http://www.erlang.org/";
    downloadPage = "http://www.erlang.org/download.html";
    description = "Programming language used for massively scalable soft real-time systems";

    longDescription = ''
      Erlang is a programming language used to build massively scalable
      soft real-time systems with requirements on high availability.
      Some of its uses are in telecoms, banking, e-commerce, computer
      telephony and instant messaging. Erlang's runtime system has
      built-in support for concurrency, distribution and fault
      tolerance.
    '';

    platforms = platforms.unix;
    maintainers = with maintainers; [ the-kenny sjmackenzie couchemar ];
    license = licenses.asl20;
  };
}
