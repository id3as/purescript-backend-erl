# Pinned development / CI toolchain.
#
# Enter with `nix-shell`, then run `spago test -- --run --filter '*'`.
# CI (.github/workflows/ci.yml) uses this same shell, so a green local run
# in `nix-shell` should mean a green CI run.
let
  # nixos-25.05 (nodejs 22, erlang 26), as a hashed tarball rather than a
  # git clone so that CI does not have to fetch the whole nixpkgs history.
  pinnedNix = builtins.fetchTarball {
    name = "nixpkgs-pinned";
    url = "https://github.com/NixOS/nixpkgs/archive/ac62194c3917d5f474c1a844b6fd6da2db95077d.tar.gz";
    sha256 = "0v6bd1xk8a2aal83karlvc853x44dg1n4nk08jg3dajqyy0s98np";
  };

  # Exact purs / spago releases (nixpkgs itself lags behind on spago).
  purescriptOverlay = builtins.fetchGit {
    name = "purescript-overlay";
    url = "https://github.com/thomashoneyman/purescript-overlay.git";
    ref = "main";
    rev = "1cf88ab9d83596db0e0c0d304a16809c410e2917";
  };

  # purerl releases, used to build the test-snapshots package
  # (test-snapshots/spago.yaml declares `backend: cmd: purerl`).
  purerlReleases = builtins.fetchGit {
    name = "nixpkgs-purerl";
    url = "https://github.com/purerl/nixpkgs-purerl.git";
    ref = "master";
    rev = "69ea3146f3c4f715c5dbc6e0f8ba7d0ee57bb3bd";
  };

  nixpkgs = import pinnedNix {
    overlays = [
      (import "${purescriptOverlay}/overlay.nix")
      (import purerlReleases)
    ];
  };

  erlangChannel = nixpkgs.beam.packages.erlang_26;
in
with nixpkgs;

mkShell {
  buildInputs = [
    nodejs_22

    # PureScript compiler and build tool; spago 0.93.45 is the version that wrote
    # the committed spago.lock files (0.93.27 and 1.0.x rewrite them).
    purs-bin.purs-0_15_15
    spago-bin.spago-0_93_45

    # purerl backend, for building test-snapshots
    purerl.purerl-0-0-22

    # erlc / erl, for compiling and running the generated snapshot modules
    erlangChannel.erlang
  ];
}
