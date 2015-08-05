{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc7101" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, aeson, base, bytestring, classy-prelude
      , classy-prelude-conduit, classy-prelude-yesod, conduit, containers
      , data-default, directory, fast-logger, file-embed, hjsmin, hspec
      , http-conduit, lifted-base, monad-control, monad-logger, network
      , safe, shakespeare, stdenv, stm, template-haskell, text, time
      , unordered-containers, vector, wai, wai-extra, wai-logger, warp
      , websockets, yaml, yesod, yesod-core, yesod-form, yesod-static
      , yesod-test, yesod-websockets
      }:
      mkDerivation {
        pname = "mybabymonitor-org";
        version = "0.0.0";
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        buildDepends = [
          aeson base bytestring classy-prelude classy-prelude-conduit
          classy-prelude-yesod conduit containers data-default directory
          fast-logger file-embed hjsmin http-conduit lifted-base
          monad-control monad-logger network safe shakespeare stm
          template-haskell text time unordered-containers vector wai
          wai-extra wai-logger warp websockets yaml yesod yesod-core
          yesod-form yesod-static yesod-websockets
        ];
        testDepends = [
          base classy-prelude classy-prelude-yesod hspec yesod yesod-core
          yesod-test
        ];
        license = "AGPL";
      };

  drv = pkgs.haskell.packages.${compiler}.callPackage f {};

in

  if pkgs.lib.inNixShell then drv.env else drv
