{
  description = "Tree-sitter grammar for Lean 4";

  nixConfig = {
    extra-substituters = [ "https://wvhulle.cachix.org" ];
    extra-trusted-public-keys = [ "wvhulle.cachix.org-1:heXx8DZMiRsKUx6l1TxNoF+Nmtmz66QEdsonQzc1ir0=" ];
  };
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  };

  outputs =
    {
      self,
      nixpkgs,
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ];

      forAllSystems = f: nixpkgs.lib.genAttrs systems (system: f nixpkgs.legacyPackages.${system});
    in
    {
      packages = forAllSystems (
        pkgs:
        let
          src = pkgs.lib.cleanSource ./.;
          version = "0.2.2";
        in
        {
          default = pkgs.tree-sitter.buildGrammar {
            language = "lean";
            inherit version src;
            postInstall = ''
              mv $out/parser $out/lean.so
            '';
          };
        }
      );

      devShells = forAllSystems (pkgs: {
        default = pkgs.mkShell {
          packages = with pkgs; [
            tree-sitter
            rustup
            nushell
          ];

          # 16 GB virtual memory limit prevents tree-sitter generate from OOMing
          # the system. GLR conflicts in the grammar can cause exponential state
          # table growth, consuming 20+ GB and killing the machine.
          shellHook = ''
            ulimit -v 16000000
          '';
        };
      });

    };
}
