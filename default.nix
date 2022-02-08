{ mkDerivation, async, base, bytestring, lib, network }:
mkDerivation {
  pname = "Chat";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ async base bytestring network ];
  executableHaskellDepends = [ async base bytestring network ];
  license = "unknown";
  hydraPlatforms = lib.platforms.none;
}
