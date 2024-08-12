# Keep this up to date with cabal.project
let
  tree-sitter-simple-repo = {
    url = "https://github.com/oberblastmeister/tree-sitter-simple";
    sha256 = "sha256-8i2NYbILmGDH+0EIQ/UzRoyuonWzyAqFD06vbjEJH2g=";
    rev = "309ac26ecbb94e6f534bff5c4c5c6a50808397e0";
    fetchSubmodules = true;
  };

in
  self: super: {
    ghcVersion = "ghc963";

    all-cabal-hashes =
        # Update revision to match required hackage
        super.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e5ab1dca27a34bbec5920863c18d8cd06b0c288d.tar.gz";
          sha256 = "sha256-qJt8ZAG/Ilx0JMyd/FTuHB7k5SpYqkLJFUCi3uEBhvU=";
    };

    haskellPackages = super.haskell.packages.${self.ghcVersion}.override {
      overrides = haskellSelf: haskellSuper: {
        tree-sitter-haskell =
          haskellSuper.callCabal2nix
          "tree-sitter-haskell" "${(super.fetchgit tree-sitter-simple-repo)}/tree-sitter-haskell" {};

        tree-sitter-simple =
          haskellSuper.callCabal2nix
          "tree-sitter-simple" "${(super.fetchgit tree-sitter-simple-repo)}/tree-sitter-simple" {};

        tree-sitter-ast =
          haskellSuper.callCabal2nix
          "tree-sitter-ast" "${(super.fetchgit tree-sitter-simple-repo)}/tree-sitter-ast" {};

        haskell-ast =
          self.haskellPackages.callCabal2nix
          "haskell-ast" "${(super.fetchgit tree-sitter-simple-repo)}/haskell-ast" {};

        text-range =
          haskellSuper.callCabal2nix
          "text-range" "${(super.fetchgit tree-sitter-simple-repo)}/text-range" {};

        tasty-expect = haskellSuper.callCabal2nix "tasty-expect" (super.fetchgit {
          url = "https://github.com/oberblastmeister/tasty-expect.git";
          sha256 = "sha256-KxunyEutnwLeynUimiIkRe5w/3IdmbD/9hGVi68UVfU=";
          rev = "ec14d702660c79a907e9c45812958cd0df0f036f";
        }) {};

        hiedb = self.haskell.lib.dontCheck (haskellSuper.callHackage "hiedb" "0.6.0.1" {});
        text-rope = haskellSuper.callHackage "text-rope" "0.2" {};

        lsp = haskellSuper.callHackage "lsp" "2.4.0.0" {};
      };
    };
  }
