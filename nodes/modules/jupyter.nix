{
  config,
  lib,
  pkgs,
  ...
}:
let
  python3 =
    let
      env = (
        pkgs.python311.withPackages (
          pythonPackages: with pythonPackages; [
            ipykernel
            pip
            numpy
            pymc
            pandas
            scikit-learn
            jax
            jaxlib
          ]
        )
      );
    in
    {
      displayName = "Python 3 for machine learning";
      argv = [
        "${env.interpreter}"
        "-m"
        "ipykernel_launcher"
        "-f"
        "{connection_file}"
      ];
      language = "python";
#      logo32 = ./. + "${env.sitePackages}/ipykernel/resources/logo-32x32.png";
#      logo64 = ./. + "${env.sitePackages}/ipykernel/resources/logo-64x64.png";
      extraPaths = {
        "cool.txt" = pkgs.writeText "cool" "cool content";
      };
    };
in
{
  users.users.jupyter.group = "jupyter";
  users.groups.jupyter = {};
  services.jupyter = {
    enable = true;
    command = "jupyter-lab";
    password = "'sha1:1b961dc713fb:88483270a63e57d18d43cf337e629539de1436ba'";
    group = "jupyter";
    package = pkgs.python311Packages.jupyterlab;
    kernels = {
      ml = python3;
    };
#    notebookDir = "./Sources/notebooks";
  };
}
