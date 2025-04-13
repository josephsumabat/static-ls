# Keep this up to date with cabal.project
let
  tree-sitter-simple-repo = {
    url = "https://github.com/josephsumabat/tree-sitter-simple";
    sha256 = "sha256-X4SM6v9CMs3DeX7thhV+8bEmVxDNdMGaqHr9t0dDdYw=";
    rev = "03085cb9e6f1c7850e0c3bcd227ba0c730c7013e";
    fetchSubmodules = true;
  };

  haskell-arborist-repo = {
    url = "https://github.com/josephsumabat/haskell-arborist";
    sha256 = "sha256-7KhlcDsrdxHwEl3FCa6WHneP7En6Luqfd9mUQw+BrP4=";
    rev = "d69de64fcf2cd729816b18f1a0a410c198de62af";
    fetchSubmodules = true;
  };

  # GHC 9.12 test fixes merged upstream (well-typed/optics#512) but no
  # Hackage release yet; tracked in GHC #25631 and GHC #25883
  optics-repo = {
    url = "https://github.com/well-typed/optics";
    sha256 = "sha256-1Eoyaf0KUEtNL48Lb78NTSpx5LJaDDInguea306neXI=";
    rev = "d3f507217bb828ec97d3ead69b0b4319941c0ec5";
  };

in
  ghcVersion:
  self: super: {
    ghcVersion = ghcVersion;

    all-cabal-hashes =
        # Update revision to match required hackage
        super.fetchurl {
          url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/e82a78bdc7a453ec96d5d8879bb9862b63a6f1b5.tar.gz";
          sha256 = "sha256-gV4iJvIfwSSrdYJeHQ6d8qchRP9hRGP2pj8xMkcJoCQ=";
    };

    haskellPackages = super.haskell.packages.${self.ghcVersion}.override {
      overrides = haskellSelf: haskellSuper: {
        haskell-arborist =
          haskellSuper.callCabal2nix
          "haskell-arborist" "${(super.fetchgit haskell-arborist-repo)}" {};

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

        hiedb = self.haskell.lib.dontCheck (haskellSuper.callHackage "hiedb" "0.6.0.2" {});

        text-rope = haskellSuper.callHackage "text-rope" "0.3" {};

        lsp-types = haskellSuper.callHackage "lsp-types" "2.3.0.1" {};

        lsp = haskellSuper.callHackage "lsp" "2.7.0.1" {};

        optics = self.haskell.lib.dontCheck (
          haskellSuper.callCabal2nix
          "optics" "${(super.fetchgit optics-repo)}/optics" {});
      };
    };
  }
