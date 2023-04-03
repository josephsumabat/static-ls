# static-ls

static-ls is a hie and hiedb based language server heavily inspired by `halfsp`.

The goal of static-ls is to provide a high-speed, low-memory solution for large
projects for which haskell-language-server tends to take up too much memory on
recompilation. Haskell-language-server is recommended if you are not
experiencing these issues. `static-ls` is meant to work on enterprise size
projects. `static-ls` tends to work better as a code navigation tool since
project edits require re-indexing of hie files.

In the future we plan to use interface files to fetch documentation information
as well as possibly other static sources of information

## Usage

1. Compile your project with ide info `-fwrite-ide-info` and `-hiedir .hiefiles`
  - If you're using hpack you can add:
    ```
      ghc-options:
        - -fwrite-ide-info
        - -hiedir .hiefiles
    ```
    to  your `package.yaml`
    See this project's `package.yaml` or `static-ls.cabal` for examples
2. Index your project in hiedb running `hiedb -D .hiedb index .hiefiles --src-base-dir .`
    from your workspace root
4. Point your language client to the `static-ls` binary and begin editting!
    (See [Editor Setup](#editor-setup) for instructions if you're not sure how)

`ghcid` is recommended to refresh hie files

## Features

static-ls supports the following lsp methods:
- `textDocument/references`
- `textDocument/hover`
- `textDocument/definition`

## Limitations
- Must be compiled on the same version of ghc as the project
- You will need to re-index your hie files once you edit your project

## Editor setup
Instructions for editor setup

### neovim - coc.nvim
call `:CocConfig` and copy the following in:
```
{
  "languageserver": {
    "static-ls": {
      "command": "static-ls",
      "rootPatterns": ["*.cabal", "stack.yaml", "cabal.project", "package.yaml", "hie.yaml"],
      "filetypes": ["haskell"]
    },
  },
}
```
