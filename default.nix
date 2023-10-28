{ haskellPackages }:

let
  drv = haskellPackages.callPackage ({ mkDerivation, base, brick, containers, directory, filepath
      , lens, lib, linear, optparse-applicative, random, transformers
      , vty, mtl, async, criterion, hspec, hspec-discover, QuickCheck, aeson
      }:
      mkDerivation {
        pname = "tetris";
        version = "0.1.4.1";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          base brick containers lens linear random transformers vty mtl async criterion hspec hspec-discover QuickCheck aeson
        ];
        executableHaskellDepends = [
          base directory filepath optparse-applicative
        ];
        homepage = "https://github.com/samtay/tetris#readme";
        license = lib.licenses.bsd3;
      }) {};

  # src = fetchgit { url = "https://github.com/siers/tetris"; hash = "sha256-N8PG/bIpF/rUaqzhqBKn/z2Fjk8SVcsEpSPacCSXSRI="; };
in
  drv
