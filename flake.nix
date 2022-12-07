{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  inputs.nixpkgs.url = "nixpkgs";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = nixpkgs.legacyPackages.${system};
      in {
        devShells.default = pkgs.mkShell {
          nativeBuildInputs = [ pkgs.bashInteractive ];
          buildInputs = with pkgs; [
            (ghc.withPackages (hpkgs: [
              hpkgs.advent-of-code-api
              hpkgs.split
              hpkgs.regex-tdfa
              hpkgs.raw-strings-qq
            ]))
            (python310.withPackages (pyPkgs: with pyPkgs; [ aocd pygame ]))
          ];
        };
      });
}
