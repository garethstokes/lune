# Lune LSP

Lune ships a basic Language Server Protocol (LSP) server inside the `lune` CLI.

## Running

Start the server over stdin/stdout:

```bash
lune lsp --stdio
```

`--stdio` is the default, so `lune lsp` also works.

## Features (current)

- Diagnostics on open/change (parse + type errors)
- Document formatting via `textDocument/formatting` (same formatter as `lune --fmt`)

## Neovim (nvim-lspconfig)

```lua
-- Ensure *.lune files use the "lune" filetype
vim.filetype.add({ extension = { lune = "lune" } })

local configs = require("lspconfig.configs")
local lspconfig = require("lspconfig")
local util = require("lspconfig.util")

if not configs.lune then
  configs.lune = {
    default_config = {
      cmd = { "lune", "lsp", "--stdio" },
      filetypes = { "lune" },
      root_dir = util.root_pattern("lune.cabal", "cabal.project", ".git"),
      single_file_support = true,
    },
  }
end

lspconfig.lune.setup({})
```

Formatting:

```lua
vim.lsp.buf.format({ async = true })
```

## VS Code

VS Code doesn’t ship a generic external LSP client by default. Install a “generic LSP client” extension that can launch a stdio server, then configure it to run:

- Command: `lune`
- Args: `lsp`, `--stdio`
- Language / file pattern: `*.lune`

Most generic LSP extensions accept a config shaped like:

```json
{
  "command": "lune",
  "args": ["lsp", "--stdio"],
  "languages": ["lune"]
}
```

Adjust the exact JSON/settings keys to match the extension you choose.

## Limitations

- No hover, go-to-definition, completion, rename, references, code actions, symbols, or semantic tokens yet.
- The server currently recomputes diagnostics per file change (no incremental analysis engine).
