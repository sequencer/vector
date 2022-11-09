{
  description = "vector";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        deps = pkgs.callPackage ./deps.nix {};
      in
        {
          devShell = pkgs.mkShellNoCC {
            buildInputs = deps;
            shellHook = ''
              # waiting for https://github.com/NixOS/nixpkgs/pull/192943
              export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:${pkgs.lib.makeLibraryPath [pkgs.systemd]}

              export NIX_CC=" "
            '';
          };
        }
      );
}
