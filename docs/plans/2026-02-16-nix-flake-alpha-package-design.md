# Nix Flake Alpha Package Design

## Goal

Create a self-contained Nix flake that lets alpha testers run:

```bash
nix develop github:garethstokes/lune
```

And immediately have access to:
- `lune build` - compile to Go executables
- `lune --fmt` - format Lune source files
- `lune lsp` - language server for editor integration
- `go` - for building the generated code

## Design Decisions

- **Audience:** Close contacts who can be supported directly
- **Distribution:** Nix flake with devShell + package outputs
- **Path resolution:** Environment variables (`LUNE_PRELUDE_PATH`, `LUNE_RUNTIME_PATH`)
- **Haskell build:** Via `github:garethstokes/haskell.flake`

## Flake Outputs

| Output | Command | Description |
|--------|---------|-------------|
| `devShells.default` | `nix develop github:garethstokes/lune` | Full dev environment |
| `packages.default` | `nix run github:garethstokes/lune` | Run lune directly |

## Components Bundled

| Component | Source | Purpose |
|-----------|--------|---------|
| `lune` | Built from repo | The compiler (wrapped with env vars) |
| `go` | nixpkgs | Go compiler for building generated code |
| Prelude | `./prelude/` | Standard library (in Nix store) |
| Runtime | `./runtime/go/` | Go runtime support (in Nix store) |

## Implementation Steps

### 1. Compiler Changes

**`src/Lune/ModuleGraph.hs`** - Add `LUNE_PRELUDE_PATH` support:
- Check env var in `resolveModulePath`
- If set, use that path instead of hardcoded `"prelude"`
- Fall back to current behavior if unset

**`app/Main.hs`** - Add `LUNE_RUNTIME_PATH` support:
- Check env var in `buildGo` function
- If set, use that path instead of `"runtime/go"`
- Fall back to current behavior if unset

### 2. Flake Changes

Update `flake.nix`:

```nix
{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    haskell-env.url = "github:garethstokes/haskell.flake";
  };

  outputs = { self, nixpkgs, flake-utils, haskell-env }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Build the lune binary via haskell.flake
        lune = <cabal build via haskell-env>;

        # Bundle prelude and runtime
        lune-prelude = pkgs.runCommand "lune-prelude" {} ''
          cp -r ${./prelude} $out
        '';

        lune-runtime = pkgs.runCommand "lune-runtime" {} ''
          cp -r ${./runtime/go} $out
        '';

        # Wrapper script that sets env vars
        lune-wrapped = pkgs.writeShellScriptBin "lune" ''
          export LUNE_PRELUDE_PATH="${lune-prelude}"
          export LUNE_RUNTIME_PATH="${lune-runtime}"
          exec ${lune}/bin/lune "$@"
        '';
      in {
        packages.default = lune-wrapped;

        devShells.default = pkgs.mkShell {
          packages = [ lune-wrapped pkgs.go ];
        };
      }
    );
}
```

### 3. Testing

1. Test locally: `nix develop .` and `nix run .`
2. Test remote: `nix develop github:garethstokes/lune` after pushing
