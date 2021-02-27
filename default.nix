{ chan ? "5272327b81ed355bbed5659b8d303cf2979b6953"
, compiler ? "ghc865"
, withHoogle ? false
, doHoogle ? false
, doHaddock ? false
, enableLibraryProfiling ? false
, enableExecutableProfiling ? false
, strictDeps ? false
, isJS ? false
, asShell ? false
, system ? builtins.currentSystem
, optimize ? true
}:
  let
    # Additional ignore patterns to keep the Nix src clean
    ignorance =
      [ "*.md"
        "*.adoc"
        "intro"
        "*.nix"
        "*.sh"
        "*.yml"
      ];

    # Important for a clean nix source
    gitignore =
      (pkgs.callPackage
        (pkgs.fetchFromGitHub
          { owner = "siers";
            repo = "nix-gitignore";
            rev = "4f2d85f2f1aa4c6bff2d9fcfd3caad443f35476e";
            sha256 = "1vzfi3i3fpl8wqs1yq95jzdi6cpaby80n8xwnwa8h2jvcw3j7kdz";
          }
        )
        {}
      ).gitignoreSource;

    # Build faster by doing less
    chill = p:
      (pkgs.haskell.lib.overrideCabal p
        { inherit enableLibraryProfiling enableExecutableProfiling; }
      ).overrideAttrs (_: { inherit doHoogle doHaddock strictDeps; });

    # Haskell specific overlay (for you to extend)
    haskell-overlay = hself: hsuper:
      { # "happy" = pkgs.haskell.lib.dontCheck hsuper.happy;
      };

    # Top level overlay (for you to extend)
    nix-formatter-overlay = self: super:
      { # google-chrome = (import (builtins.fetchTarball {
        #   url = "https://github.com/NixOS/nixpkgs/archive/71336116f3f78d3bb1f499bf4b88efcd8738a9cf.tar.gz";
        # }) {}).google-chrome;

        haskell =
          super.haskell //
          { packages =
              super.haskell.packages //
              { ${compiler} =
                  super.haskell.packages.${compiler}.override
                    (old:
                      { overrides =
                          super.lib.composeExtensions
                            (old.overrides or (_: _: {}))
                            haskell-overlay;
                      }
                    );
              };
          };
      };

    # Complete package set with overlays applied
    pkgs =
      import
        (builtins.fetchTarball
          { url = "https://github.com/NixOS/nixpkgs/archive/${chan}.tar.gz"; }
        )
        { inherit system;
          overlays = [ nix-formatter-overlay ];
        };

    ghcTools = with pkgs.haskell.packages.${compiler}; [ cabal-install ghcid ];

    nix-formatter =
      pkgs.haskell.packages.${compiler}.callCabal2nix "nix-formatter"
      (gitignore ignorance ./.) {};
  in
    with pkgs;
    with lib;

    if asShell then
      pkgs.haskell.packages.${compiler}.shellFor
        { inherit withHoogle;
          packages = _: [ nix-formatter ];
          COMPILER = compiler;
          buildInputs = ghcTools;
          shellHook = "cat ${./intro} ";
        }
    else
      chill nix-formatter
