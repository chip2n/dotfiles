{ pkgs, inputs, ... }:

{
  home.packages = [
    inputs.zig.packages.${pkgs.system}."master-2026-04-01"
    inputs.zls.packages.${pkgs.system}.zls
  ];
}
