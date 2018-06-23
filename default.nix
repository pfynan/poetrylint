{ reflex-platform ? import ./reflex-platform {} }:

reflex-platform.project ({ pkgs, ... }: {
  useWarp = true;
  packages = {
    common = ./common;
    pronounce = ./pronounce;
    backend = ./backend;
    frontend = ./frontend;
  };

  shells = {
    ghc = ["pronounce" "common" "backend" "frontend"];
    ghcjs = ["pronounce" "common" "frontend"];
  };

  # shellToolOverrides = ghc: super: {
  #    network = reflex-platform.ghc.network;
  #    sws = reflex-platform.ghc.sws;
  #   ghc-mod = null;
  #   haskell-ide-engine = ((import ./hie-nix/ghc-8.0.nix { inherit pkgs; }).override {
  #   overrides = self: super: {
  #     # TODO: unnecessary with https://github.com/input-output-hk/stack2nix/issues/84#issuecomment-362035573
  #     Cabal = null;
  #   };
  # }).haskell-ide-engine;
  # };
})
