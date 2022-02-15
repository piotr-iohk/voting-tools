{ inputs }:

# our packages overlay
pkgs: _: with pkgs;
  let
    compiler = config.haskellNix.compiler or "ghc8107";
  in {
  votingToolsHaskellPackages = import ./haskell.nix {
    inherit compiler
      pkgs
      lib
      stdenv
      haskell-nix
      buildPackages
      gitrev
      ;
  };

  nixosTests = import ./nixos/tests {
    inherit inputs pkgs;
  };
}
