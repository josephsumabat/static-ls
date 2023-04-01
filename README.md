# static-ls

static-ls is a hie and hiedb based language server heavily inspired by halfsp.

The goal of static-ls is to provide a high-speed, low-memory solution for large
projects which make extensive use of template Haskell (and thus where
haskell-language-server tends to take up too much memory on recompilation).
Haskell-language-server is recommended if you are not experiencing these
issues. `static-ls` is meant to work on enterprise size projects.

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
    See this project's `package.yaml` for an example
2. Index your project in hiedb running `hiedb -D .hiedb index .hiefiles`
3. Point your language client to the `static-ls` binary and begin editting!

`ghcid` is recommended if you want

## Features

static-ls supports:
- `textDocument/references`
- `textDocument/hover`

## Limitations
- Must be compiled on the same version of ghc as the project
- You will need to re-index your hie files once you edit your project
