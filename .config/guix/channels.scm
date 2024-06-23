(list
 ;; Personal channel
 (channel
  (name 'chip)
  (url "https://github.com/chip2n/guix-channel.git")
  (branch "main")
  (commit "851d24395445399e2b10c3bad94f141d20ce1c34"))
 ;; Guix channel
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (branch "master")
  (commit "d69270b696a9badd0ba91fd1ec94f6f292ac1a53")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA"))))
 ;; NonGuix channel
 (channel
  (name 'nonguix)
  (url "https://gitlab.com/nonguix/nonguix")
  (branch "master")
  (commit "877ed8cc4eee26ddde3d7d200c19c370c6bf7cb1")
  ;; Enable signature verification:
  (introduction
   (make-channel-introduction
    "897c1a470da759236cc11798f4e0a5f7d4d59fbc"
    (openpgp-fingerprint
     "2A39 3FFF 68F4 EF7A 3D29  12AF 6F51 20A0 22FB B2D5")))))
