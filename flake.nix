{
  description = "Rust-GPU";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    rust-overlay.url = "github:oxalica/rust-overlay";
    naersk-rust-builder.url = "github:nix-community/naersk";
  };

  outputs =
    {
      nixpkgs,
      rust-overlay,
      naersk-rust-builder,
      ...
    }:
    let
      system = "x86_64-linux";
      overlays = [ (import rust-overlay) ];
      pkgs = import nixpkgs { inherit system overlays; };

      toolchain =
        let
          path = ./rust-toolchain.toml;
        in
        if builtins.pathExists path then
          (builtins.fromTOML (builtins.readFile path)).toolchain
        else
                 throw "rust-toolchain.toml not found at' ${path}'.";

      channelStr = toolchain.channel;

      dashIndex =
        let
          idx = builtins.stringLength (builtins.head (builtins.split "-" channelStr));
        in
        if builtins.match ".*-.*" channelStr == null then 
         throw "Invalid Rust channel: '${channelStr}'. Expected format like 'nightly-YYYY-MM-DD'."
        else idx;

      channelInfo =
          let
            channelType = builtins.substring 0 dashIndex channelStr;
            channelDate = builtins.substring (dashIndex + 1) (
              builtins.stringLength channelStr - dashIndex - 1
            ) channelStr;
          in
          {
            inherit channelType channelDate;
          };

      rustToolchain =
          (pkgs.rust-bin.${channelInfo.channelType}.${channelInfo.channelDate}.default.override {
            extensions = toolchain.components;
          });

      naersk-lib = pkgs.callPackage naersk-rust-builder {
        cargo = rustToolchain;
        rustc = rustToolchain;
      };

    in
    {
      packages.${system}.default = naersk-lib.buildPackage {
        src = ./.;
      };

      devShells.${system}.default = pkgs.mkShell {
        buildInputs = [
          pkgs.rust-analyzer
          pkgs.clippy
          pkgs.rustfmt
          pkgs.pkg-config
          rustToolchain
        ];

        #RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
      };
    };
}
