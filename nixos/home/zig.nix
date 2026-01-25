{ pkgs, inputs, ... }:

{
  home.packages = [
    inputs.zig.packages.${pkgs.system}."master-2025-12-23"
    inputs.zls.packages.${pkgs.system}.zls
  ];
}
