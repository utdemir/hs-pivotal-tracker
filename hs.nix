{ mkDerivation, aeson, aeson-casing, base, either, hspec
, http-client, http-client-tls, mtl, servant, servant-client
, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "pivotal-tracker";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base http-client http-client-tls mtl servant
    servant-client text time transformers
  ];
  executableHaskellDepends = [
    base either servant text transformers
  ];
  testHaskellDepends = [
    base either hspec servant text transformers
  ];
  description = "A library and a CLI tool for accessing Pivotal Tracker API";
  license = stdenv.lib.licenses.bsd3;
}
