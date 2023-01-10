{
  description = "doorctl-api";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [
      flake-utils.lib.system.aarch64-linux
      flake-utils.lib.system.x86_64-linux
    ] (system:
      let
        overlay = final: prev: {
          haskellPackages = prev.haskell.packages.ghc944.override {
            overrides = hsFinal: hsPrev: {
              cborg = hsPrev.cborg.overrideAttrs (old: {
                doHaddock = false;
                # See: https://github.com/well-typed/cborg/pull/304
                patches = ./patches/cborg-ghc944.patch;
              });
              servant = final.haskell.lib.overrideCabal hsPrev.servant (old: {
                doHaddock = false;
                doCheck = false; # Depends on hspec <2.10, and nixpkgs is newer
              });
            };
          };
        };

        pkgs = nixpkgs.legacyPackages.${system}.appendOverlays([ overlay ]);

        haskellPackages = pkgs.haskellPackages;

        packageName = "doorctl-api";

        t = pkgs.lib.trivial;
        hl = pkgs.haskell.lib;

        project = devTools: isDoorController:
          let addBuildTools = (t.flip hl.addBuildTools) devTools;
          in haskellPackages.developPackage ({
            root = ./.;
            name = packageName;
            returnShellEnv = !(devTools == [ ]);

            modifier = (t.flip t.pipe) [
              addBuildTools
              hl.dontHaddock
              hl.disableLibraryProfiling
              hl.disableExecutableProfiling
            ];

            source-overrides = {
              attoparsec-iso8601 = "1.1.0.0";
              http-api-data = "0.5";
            } // pkgs.lib.optionalAttrs (!isDoorController) {
              postgresql-simple = "0.6.5";
            };
          } // pkgs.lib.optionalAttrs isDoorController {
            cabal2nixOptions = pkgs.lib.concatStringsSep " " [
              "--flag controller"
            ];
          });
      in {
        overlays.default = overlay;

        packages.${packageName} = project [ ] false;

        packages."${packageName}-controller" = project [ ] true;

        defaultPackage = self.packages.${system}.${packageName};

        devShell = project (with pkgs; [
          ghcid
          cabal-install
        ]);
      });
}
