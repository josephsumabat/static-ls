# static-ls

static-ls ("static language server") is a [hie files](https://gitlab.haskell.org/ghc/ghc/-/wikis/hie-files) and
[hiedb](https://github.com/wz1000/HieDb/) based language server heavily
inspired by [halfsp](https://githubcom/masaeedu/halfsp), which reads static
project information to provide IDE functionality through the language server
protocol. `static-ls` will not generate this information on its own and instead
will rely on the user to generate this information via separate programs

Supported static sources of information currently include:
- hiefiles
- hiedb

The goal of `static-ls` is to provide a high-speed, low-memory solution for large
projects for which
[haskell-language-server](https://github.com/haskell/haskell-language-server)
tends to take up too much memory on recompilation.
[Haskell-language-server](https://github.com/haskell/haskell-language-server)
is recommended if you are not experiencing these issues. `static-ls` is meant
to work on enterprise size projects where aforementioned constraints can be an
issue. `static-ls` tends to work better standalone as a code navigation tool
since project edits require re-indexing of hie files but also works reasonably
well for editing with a program such as
[ghcid](https://github.com/ndmitchell/ghcid) to watch files for compilation and
the `-fdefer-type-errors flag`.

In the future we plan to use more sources of static information such as interface files
to fetch documentation or ghcid's output to fetch diagnostics

Currently only ghc 9.4.4 and 9.6.1 are explicitly supported but I'm happy to add support for other versions of ghc if desired

## Usage

1. Compile your project with ide info `-fwrite-ide-info` and `-hiedir .hiefiles`
    
    For a better UX, the following flags are *strongly* recommended.

     ```
     - -fdefer-type-errors
     - -Werror=deferred-type-errors
     - -Werror=deferred-out-of-scope-variables
     - -fno-defer-typed-holes
     ```
  
    These flags will allow hie files to be refreshed even if compilation fails to
    type check and will ensure that type check failures are still thrown as
    errors.

    - If you're using hpack you can add:
      ```
        ghc-options:
          - -fwrite-ide-info
          - -hiedir .hiefiles
          - -fdefer-type-errors
          - -Werror=deferred-type-errors
          - -Werror=deferred-out-of-scope-variables
          - -fno-defer-typed-holes
      ```
      to  your `package.yaml`. See this project's `package.yaml` or `static-ls.cabal` for examples
    - You may instead add the following to your `cabal.project.local` file:
      ```
      ignore-project: False
      program-options:
        ghc-options:
          -fdefer-type-errors
          -Werror=deferred-type-errors
          -Werror=deferred-out-of-scope-variables
          -fno-defer-typed-holes
      ```
    
2. Index your project in hiedb running:
      ```
        hiedb -D .hiedb index .hiefiles --src-base-dir .
      ```

    from your workspace root. If you're on an older version of `hiedb` where the `--src-base-dir` argument is not available use:
    
      ```
        hiedb -D .hiedb index .hiefiles
      ```
3. Point your language client to the `static-ls` binary and begin editing!
    (See [Editor Setup](#editor-setup) for instructions if you're not sure how)

[ghcid](https://github.com/ndmitchell/ghcid) is recommended to refresh hie files but compiling with `cabal build` should work as well

## Features

`static-ls` supports the following lsp methods:
- `textDocument/references`
  - Note that find references only works on top level definitions and can be
    slow for functions which are used frequently

![Find references](./docs/gifs/find-references.gif)

- `textDocument/hover`
  - Provides type information and definition location on hover

![Type on hover](./docs/gifs/hover.gif)

- `textDocument/definition`
  - Works on both local and top level definitions

![Find definition](./docs/gifs/find-definition.gif)

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
