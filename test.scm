(use gauche.test)
(use gauche.uvector)
(use gauche.sequence)
(use rfc.json)
(use rfc.base64)

(test-start "blake3")
(use blake3)
(test-module 'blake3)

(define (b3hex s)
  (digest-message-to 'hex <blake3> s))

(define (b3hex-len s len)
  (let1 ctx (make <blake3>)
    (digest-update! ctx s)
    (base16-encode-message (digest-final! ctx len) :lowercase #t)))

(define (b3hex-len-keyed s key len)
  (let1 ctx (make <blake3> :keyed key)
    (digest-update! ctx s)
    (base16-encode-message (digest-final! ctx len) :lowercase #t)))

(define (b3hex-len-derived s context len)
  (let1 ctx (make <blake3> :context context)
    (digest-update! ctx s)
    (base16-encode-message (digest-final! ctx len) :lowercase #t)))

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

(test-section "different digest lengths")

(test* "length 0"
       ""
       (b3hex-len "foo bar baz\n" 0))

(test* "length 16"
       "6af5038f93c6931183f41319a18bf8da"
       (b3hex-len "foo bar baz\n" 16))

(test* "length 32"
       "6af5038f93c6931183f41319a18bf8dad599e394c750f9134bbb18046d586959"
       (b3hex-len "foo bar baz\n" 32))

(test* "length 64"
       "6af5038f93c6931183f41319a18bf8dad599e394c750f9134bbb18046d586959c75672329ae0b843580e2d8aefcccff7fb4fe22c3125e366adf136f89886cc5a"
       (b3hex-len "foo bar baz\n" 64))

(test* "length 64 has length 128 as hex"
       128
       (string-length (b3hex-len "foo bar baz\n" 64)))

(let1 ctx (make <blake3>)
  (test* "invalid digest length" (test-error) (digest-final! ctx -1))
  (test* "invalid digest length" (test-error) (digest-final! ctx 'foo)))

(test-section "official test vectors: hashed")

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
            (digest-message-to 'hex <blake3> input))
     (test* #"input-len ~input-len full"
            expected-hash
            (b3hex-len input (/ (string-length expected-hash) 2)))))
 (~ test-vectors "cases"))

(test-section "official test vectors: keyed")

(for-each
 (^[case]
   (let* ((input-len (~ case "input_len"))
          (expected-hash (~ case "keyed_hash"))
          (input (make-u8vector input-len)))
     (u8vector-multi-copy! input 0
                           (u8vector-length input-template) input-template)
     (test* #"input-len ~input-len full"
            expected-hash
            (b3hex-len-keyed input
                             (string->u8vector (~ test-vectors "key"))
                             (/ (string-length expected-hash) 2)))))
 (~ test-vectors "cases"))

(test-section "official test vectors: derived")

(for-each
 (^[case]
   (let* ((input-len (~ case "input_len"))
          (expected-hash (~ case "derive_key"))
          (input (make-u8vector input-len)))
     (u8vector-multi-copy! input 0
                           (u8vector-length input-template) input-template)
     (test* #"input-len ~input-len full"
            expected-hash
            (b3hex-len-derived input
                               (~ test-vectors "context_string")
                               (/ (string-length expected-hash) 2)))))
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
