(use blake3)

(print (digest-message-to 'hex <blake3> "hello, world\n"))
(print (with-input-from-file "/usr/bin/gcc" (^[] (digest-to 'hex <blake3>))))
