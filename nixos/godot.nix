{ pkgs, ... }:

let
  version = "4.7-stable";
  runtimeLibs = with pkgs; [
    vulkan-loader
    libGL
    xorg.libX11
    xorg.libXcursor
    xorg.libXinerama
    xorg.libXrandr
    xorg.libXrender
    xorg.libXi
    xorg.libXext
    xorg.libXfixes
    libxkbcommon
    alsa-lib
    libpulseaudio
    dbus
    fontconfig
    udev
    speechd
    wayland
  ];
in pkgs.stdenv.mkDerivation {
  inherit version;
  pname = "godot";

  src = pkgs.fetchurl {
    url = "https://github.com/godotengine/godot/releases/download/${version}/Godot_v${version}_linux.x86_64.zip";
    sha256 = "0b1a6c54c2c619c12e169fe9241edda4b81080b519451cec2984bf0d2c6cb73c";
  };

  nativeBuildInputs = with pkgs; [ unzip autoPatchelfHook makeWrapper ];

  buildInputs = runtimeLibs;

  sourceRoot = ".";

  installPhase = ''
    mkdir -p $out/bin
    cp Godot_v${version}_linux.x86_64 $out/bin/.godot-unwrapped
    chmod +x $out/bin/.godot-unwrapped
    makeWrapper $out/bin/.godot-unwrapped $out/bin/godot \
      --prefix LD_LIBRARY_PATH : "${pkgs.lib.makeLibraryPath runtimeLibs}"
  '';
}
