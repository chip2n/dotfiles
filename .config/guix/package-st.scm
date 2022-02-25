(use-modules (guix packages)
             (guix git-download)
             (gnu packages suckless))

(package
 (inherit st)
 (source (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/chip2n/st")
                (commit "af159459d8d7edf99bdc40ce15ba37867ea5dd06")))
          (sha256
           (base32 "053m8jxv3l3d8hzx7xc8yx5r25wv003qsiwyhhq9z2qqnl7gabf8"))))
 (home-page "https://github.com/chip2n/st"))
