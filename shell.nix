{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, async, base, bytestring, lib, network }:
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
    };

  haskellPackages = if compiler == "default"
                    then pkgs.haskellPackages
                    else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

if pkgs.lib.inNixShell then drv.env else drv
