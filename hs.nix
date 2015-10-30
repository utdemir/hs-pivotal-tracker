{ mkDerivation, aeson, aeson-casing, base, either, servant
, servant-client, stdenv, text, time, transformers
}:
mkDerivation {
  pname = "tracker";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson aeson-casing base servant servant-client text time
    transformers
  ];
  executableHaskellDepends = [
    base either servant text transformers
  ];
  description = "A library and a CLI tool for accessing Pivotal Tracker API";
  license = stdenv.lib.licenses.bsd3;
}
