{
  description = "Lune";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-env.url = "path:/home/gareth/code/flakes/haskell.flake";
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
      in
      {
        devShells.default = haskellLib.mkHaskellShell {
          additionalPackages = with pkgs; [
            # Native dependencies for Haskell libs
            zlib
            pkg-config
          ];

          additionalShellHook = ''
            # Add zlib to LD_LIBRARY_PATH for Haskell native dependencies
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.zlib ]}''${LD_LIBRARY_PATH:+:$LD_LIBRARY_PATH}

            echo "Lune development shell"
            echo "  ghc:   $(ghc --version)"
            echo "  cabal: $(cabal --version | head -1)"
          '';
        };
      }
    );
}
