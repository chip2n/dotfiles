{ pkgs, inputs, ... }:

{
  home.packages = [
    inputs.zig.packages.${pkgs.system}."master-2026-02-16"
    inputs.zls.packages.${pkgs.system}.zls
  ];
}
