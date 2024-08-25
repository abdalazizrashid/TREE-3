{
  nixpkgs ? <nixpkgs>,
  nixos ? nixpkgs + "/nixos",
  nixosSystem ? import nixos,
}:
nixosSystem { }
