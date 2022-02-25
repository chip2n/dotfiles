(use-modules (guix packages)
             (guix download)
             (guix build-system copy)
             (guix licenses))

(package
 (name "zig")
 (version "0.10.0-dev.847+2e1c16d64")
 (source (origin
          (method url-fetch)
          (uri (string-append "https://ziglang.org/builds/zig-linux-x86_64-" version ".tar.xz"))
          (sha256
           (base32 "0ild7skilqnadch8ndvhl8wsqxzq3p9716dq5ipwx6nyi6fb6wqr"))))
 (build-system copy-build-system)
 (arguments
  `(#:install-plan '(("." "/bin/"))))
 (synopsis "Zig compiler")
 (description "A general-purpose programming language and toolchain for maintaining robust, optimal, and reusable software.")
 (home-page "https://ziglang.org")
 (license expat))
