(list
 ;; Personal channel
 (channel
  (name 'chip)
  (url "https://github.com/chip2n/guix-channel.git")
  (branch "main")
  (commit "2650a7c0227ba71cecec254b0759cbc16b29c109"))
 ;; Guix channel
 (channel
  (name 'guix)
  (url "https://git.savannah.gnu.org/git/guix.git")
  (branch "master")
  (commit "5a95cf76e1d0f9fdff5b232b42337c657b76d1d4")
  (introduction
   (make-channel-introduction
    "9edb3f66fd807b096b48283debdcddccfea34bad"
    (openpgp-fingerprint
     "BBB0 2DDF 2CEA F6A8 0D1D  E643 A2A0 6DF2 A33A 54FA")))))
