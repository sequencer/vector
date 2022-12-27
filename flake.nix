{
  description = "vector";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }@inputs:
    let
      overlay = import ./overlay.nix;
    in
    flake-utils.lib.eachDefaultSystem
      (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ overlay ]; };
        deps = with pkgs; [
          rv32-clang
          myLLVM.bintools

          cmake
          libargs glog fmt libspike zlib

          mill python3 go
          gnused coreutils gnumake gnugrep which
          parallel protobuf ninja verilator antlr4 numactl dtc
          espresso
          circt
        ];
      in
        {
          legacyPackages = pkgs;
          devShell = pkgs.mkShell.override { stdenv = pkgs.myLLVM.stdenv; } {
            buildInputs = deps;
          };
          devShells.docBuild = pkgs.mkShell {
            buildInputs = with pkgs; [
              yarn
            ];
          };
        }
      )
    // { inherit inputs; overlays.default = overlay; };
}
