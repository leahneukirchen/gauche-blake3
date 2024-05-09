(use gauche.test)
(use gauche.uvector)
(use gauche.sequence)
(use rfc.json)
(use rfc.base64)

(test-start "blake3: hash mode")
(use blake3)
(test-module 'blake3)

(define (b3hex s)
  (digest-message-to 'hex <blake3> s))

(test-section "simple test vectors")

(test* "empty string"
       "af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262" 
       (b3hex ""))
(test* "foo bar baz"
       "6af5038f93c6931183f41319a18bf8dad599e394c750f9134bbb18046d586959"
       (b3hex "foo bar baz\n"))
(test* "10000x A"
       "91532fcfefb40751373aa5242d99aac4eb185c45c69ae3015e699bf0a97c3597"
       (b3hex (make-string 10000 #\A)))

(test-section "official test vectors")

(define test-vectors
  (parameterize ([json-object-handler (cut alist->hash-table <> 'string=?)])
    (with-input-from-file "test_vectors.json" (^[] (parse-json)))))

(define input-template (list->u8vector (iota 251)))

(for-each
 (^[case]
   (let* ((input-len (~ case "input_len"))
          (expected-hash (~ case "hash"))
          (input (make-u8vector input-len)))
     (u8vector-multi-copy! input 0
                           (u8vector-length input-template) input-template)
     (test* #"input-len ~input-len"
            (substring expected-hash 0 64)
            (digest-message-to 'hex <blake3> input))))
 (~ test-vectors "cases"))

(test-section "incremental api")

(let1 ctx (make <blake3>)
  (digest-update! ctx #u8"foo ")
  (digest-update! ctx #u8"bar ")
  (digest-update! ctx #u8"baz\n")
  (test* "3 u8vectors"
         "6af5038f93c6931183f41319a18bf8dad599e394c750f9134bbb18046d586959"
         (base16-encode-message (digest-final! ctx) :lowercase #t)))

(let1 ctx (make <blake3>)
  (digest-update! ctx "foo")
  (digest-update! ctx " bar")
  (digest-update! ctx " baz\n")
  (test* "3 strings"
         "6af5038f93c6931183f41319a18bf8dad599e394c750f9134bbb18046d586959"
         (base16-encode-message (digest-final! ctx) :lowercase #t)))

(let1 ctx (make <blake3>)
  (test* "can't hash numbers" (test-error) (digest-update! ctx 1337)))

(test-end :exit-on-failure #t)
