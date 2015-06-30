{ mkDerivation, base, bytestring, extensible-exceptions
, happstack-server, hslogger, network, sendfile
, stdenv, time, unix, tls, data-default-class
}:
mkDerivation {
  pname = "happstack-server-tls-cryptonite";
  version = "0.1.0";
  src = ./.;
  buildDepends = [
    base bytestring extensible-exceptions happstack-server hslogger
    network sendfile time unix tls data-default-class
  ];
  homepage = "http://www.happstack.com/";
  description = "extend happstack-server with native https:// support (TLS/SSL)";
  license = stdenv.lib.licenses.bsd3;
}
