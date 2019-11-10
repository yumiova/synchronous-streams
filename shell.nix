let
  release = import ./release.nix;
in
release.synchronous-streams.env
