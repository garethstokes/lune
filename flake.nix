{
  description = "Lune";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-env.url = "github:garethstokes/haskell.flake";
  };

  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
      haskell-env,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellLib = haskell-env.lib.${system};

        # Use haskellPkgs from haskell.flake
        haskellPkgs = haskellLib.haskellPkgs;

        # Build lune, disabling tests (they require cabal which isn't in sandbox)
        lune = pkgs.haskell.lib.dontCheck (haskellPkgs.callCabal2nix "lune" ./. { });

        # Bundle prelude and runtime as separate derivations
        lune-prelude = pkgs.runCommand "lune-prelude" { } ''
          cp -r ${./prelude} $out
        '';

        lune-runtime = pkgs.runCommand "lune-runtime" { } ''
          cp -r ${./runtime/go} $out
        '';

        # Wrapper script that sets env vars and calls lune
        lune-wrapped = pkgs.writeShellScriptBin "lune" ''
          export LUNE_PRELUDE_PATH="${lune-prelude}"
          export LUNE_RUNTIME_PATH="${lune-runtime}"
          exec ${lune}/bin/lune "$@"
        '';
      in
      {
        packages.default = lune-wrapped;
        packages.lune = lune;
        packages.lune-wrapped = lune-wrapped;

        devShells.default = haskellLib.mkHaskellShell {
          additionalPackages = with pkgs; [
            # Native dependencies for Haskell libs
            zlib
            pkg-config
            go
          ];

          additionalShellHook = ''
            # Add zlib to LD_LIBRARY_PATH for Haskell native dependencies
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.zlib ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

            echo "Lune development shell"
            echo "  ghc:   $(ghc --version)"
            echo "  cabal: $(cabal --version | head -1)"
          '';
        };

        # Lightweight shell for alpha testers (just lune + go)
        devShells.user = pkgs.mkShell {
          packages = [
            lune-wrapped
            pkgs.go
          ];
          shellHook = ''
            echo "Lune alpha test shell"
            echo "  lune: available"
            echo "  go:   $(go version)"
          '';
        };
      }
    );
}
