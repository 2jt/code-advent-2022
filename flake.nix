{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    easy-purescript-nix = {
      url = "github:justinwoo/easy-purescript-nix";
      flake = false;
    };
  };
  outputs =
    { self
    , nixpkgs
    , flake-utils
    , easy-purescript-nix
    }:
    flake-utils.lib.eachDefaultSystem (system:
    let
      pkgs = nixpkgs.legacyPackages.${system};
      easy-ps = import easy-purescript-nix { inherit pkgs; };
    in
    {
      devShells.default = pkgs.mkShell {
        buildInputs = [
          easy-ps.purs
          easy-ps.spago
          easy-ps.purs-tidy
          easy-ps.purescript-language-server
          pkgs.nodejs
        ];
      };
    });
}
