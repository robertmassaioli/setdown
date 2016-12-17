{ nixpkgs ? import <nixpkgs> {}, compiler ? "default" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, array, base, bytestring, cmdargs
      , containers, directory, filepath, happy, mtl, split, stdenv, text
      , uuid
      }:
      mkDerivation {
        pname = "setdown";
        version = "0.1.0.1";
        sha256 = "1007cb1p8ymnm39cbk423jfgzvdk7yip54yy3ydiiqwkfy2rxs5g";
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          array base bytestring cmdargs containers directory filepath mtl
          split text uuid
        ];
        executableToolDepends = [ alex happy ];
        homepage = "http://bitbucket.org/robertmassaioli/setdown";
        description = "Treating files as sets to perform rapid set manipulation";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
