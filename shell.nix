{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, adjunctions, array, base, bytestring
      , containers, filepath, lens, linear, MonadRandom, mtl, OpenGL
      , sdl2, stdenv
      }:
      mkDerivation {
        pname = "4d-labyrinth";
        version = "0.1.0.0";
        src = ./4d-labyrinth;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          adjunctions array base bytestring containers filepath lens linear
          MonadRandom mtl OpenGL sdl2
        ];
        license = stdenv.lib.licenses.gpl3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
