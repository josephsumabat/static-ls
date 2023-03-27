# static-ls

static-ls is a hie and hiedb based language server heavily inspired by halfsp.

The goal of static-ls is to provide a low-memory solution for large projects which
make extensive use of template Haskell (and thus where haskell-language-server tends
to take up too much memory on recompilation). Haskell-language-server is recommended
if you are not experiencing these issues.

## Usage

1. Compile your project with ide info `-fwrite-ide-info` and `-hiedir .hiefiles`
  - If you're using hpack you can add:
    ```
      ghc-options:
        - -fwrite-ide-info
        - -hiedir .hiefiles
    ```
    See this project's `package.yaml` for an example
2. Index your project in hiedb running `hiedb -D .hiedb index .hiefiles`
3. Point your language client to the `static-ls` binary and begin editting!

## Features

static-ls supports:
- `textDocument/references`
- `textDocument/hover`
