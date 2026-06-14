{ pkgs, ... }:

{
  # --- Package override ---------------------------------------------------
  # The NixOS synergy module hardcodes pkgs.synergy (frozen at 1.14.6.19), so
  # the only way to ship a newer binary is to replace pkgs.synergy via overlay.
  # Synergy 1's open-source core is now deskflow; we pin it past the synergy
  # 1.20.1 / CVE security fixes. These are unreleased upstream as of the v1.26.0
  # tag (2026-02-16) and live only on main; verified present at this commit
  # (2026-06-12):
  #   - CVE-2026-41476  clipboard buffer overrun (IClipboard::unmarshall bounds check)
  #   - CVE-2026-44296  TLS handshake DoS (blocking sleep removed)
  #   - CVE-2026-41477  IPC privilege escalation (command-exec message removed; Windows-only)
  # To bump: change rev, run `nix-prefetch-url --unpack <archive>` + `nix hash to-sri`.
  nixpkgs.overlays = [
    (final: prev: {
      deskflow = prev.deskflow.overrideAttrs (old: {
        version = "1.26.0-unstable-2026-06-12";
        src = final.fetchFromGitHub {
          owner = "deskflow";
          repo = "deskflow";
          rev = "f51fb4affbd9c0bfd75268bc46a37cfdf4007b49";
          hash = "sha256-rz1OSKqPCKagCkkVsU+zQ09/QgPe3DRgQnOorCJeqBc=";
        };
        # New translations/ dir needs Qt LinguistTools.
        nativeBuildInputs = old.nativeBuildInputs ++ [ final.qt6.qttools ];
        # cmake installs docs itself now; upstream renamed README.md -> Readme.md.
        postInstall = "";
      });

      # deskflow-core is CLI-compatible with synergys/synergyc
      # (-c/-f/-a/-n/--enable-crypto/--tls-cert) and uses the same config format,
      # so expose those binary names as thin wrappers for the synergy module.
      synergy = final.symlinkJoin {
        name = "synergy-deskflow-${final.deskflow.version}";
        paths = [
          (final.writeShellScriptBin "synergys"
            ''exec ${final.deskflow}/bin/deskflow-core server "$@"'')
          (final.writeShellScriptBin "synergyc"
            ''exec ${final.deskflow}/bin/deskflow-core client "$@"'')
        ];
      };
    })
  ];

  # --- Service setup ------------------------------------------------------
  # Open port 24800 for Synergy
  networking.firewall.allowedTCPPorts = [ 24800 ];

  services.synergy.server = {
    enable = true;
    autoStart = true;
    screenName = "chipb0x";
    configFile = pkgs.writeText "synergy.conf" ''
      section: screens
        chipb0x:
        MacBook-Pro-M1:
      end

      section: links
        chipb0x:
          down = MacBook-Pro-M1
        MacBook-Pro-M1:
          up = chipb0x
      end
    '';
  };
}
