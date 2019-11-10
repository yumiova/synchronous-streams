let
  channel = builtins.fromJSON (builtins.readFile ./nixpkgs.json);

  gitRevision = rev: "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";

  nixpkgs = builtins.fetchTarball {
    name = "nixpkgs";
    url = gitRevision channel.rev;
    inherit (channel) sha256;
  };

  pkgs = import nixpkgs {};
in
{}
