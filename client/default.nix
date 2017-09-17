let
  pkgs = import <nixpkgs> {};
  stdenv = pkgs.stdenv;
in rec {
  project = stdenv.mkDerivation rec {
    name = "all-o-stasis-client";
    buildInputs = [
      pkgs.nodejs-8_x
    ];
  };
}
