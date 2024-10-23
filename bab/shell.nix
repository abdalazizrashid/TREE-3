{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  nativeBuildInputs = with pkgs; [ pkg-config ];
  buildInputs = [
    pkgs.rustc            # The Rust compiler
    pkgs.cargo            # Cargo, Rust’s package manager
    pkgs.rustfmt          # Code formatter
    pkgs.clippy           # Linter for Rust
    pkgs.rust-analyzer    # Language server for IDE integration
    # Add other dependencies here, e.g., pkgs.pkg-config, pkgs.openssl, etc.
    pkgs.openssl
  ];

  # Optional: Define environment variables or run commands on shell startup
  shellHook = ''
    export CARGO_INCREMENTAL=1
    echo "Welcome to your Rust development environment!"
  '';

  # Optional: If your project has specific environment variables
  # For example, setting the RUST_BACKTRACE environment variable
  # RUST_BACKTRACE=1
}
