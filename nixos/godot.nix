{ pkgs, ... }:

let
  version = "4.6-stable";
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
    sha256 = "6bcc59dfd1d670e918c77eae06e82b9dc5699de13d353dc3a4b3b6b307b6dc06";
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
