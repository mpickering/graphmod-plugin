let
  hostPkgs = import <nixpkgs> {};
in
  import (hostPkgs.fetchFromGitHub {
    owner = "mpickering";
    repo = "head.hackage";
		# mpickering/head.hackage ghc-plugins branch
    rev = "dd646463f8ca9f38143e22c1a69a20bcfb441fc5";
    sha256 = "04y377in1h18nnswk5inq99p039kvxvhqaw04sdryn0864lzy7l3"; })
