(define-module blake3
;  (use srfi-207)
  (use gauche.uvector)
  (extend util.digest)
  (export <blake3>))

(select-module blake3)

(define-class <blake3-meta> (<message-digest-algorithm-meta>) ())

(define-class <blake3> (<message-digest-algorithm>)
  (context)
  :metaclass <blake3-meta>
  :hmac-block-size 32)

(define-method initialize ((self <blake3>) initargs)
  (next-method)
  (let1 ctx (make <blake3-context>)
    (if-let1 key (get-keyword :keyed initargs #f)
       (%blake3-hasher-init-keyed ctx key)
       (if-let1 context (get-keyword :context initargs #f)
         (%blake3-hasher-init-derive-key-raw ctx context)
         (%blake3-hasher-init ctx)))
    (slot-set! self 'context ctx)))

(define-method digest-update! ((self <blake3>) data)
  (%blake3-hasher-update
   (slot-ref self'context) 
   (typecase data
             (<u8vector> data)
             (<string> (string->u8vector data))
             (else (error "cannot hash this:" data)))))

(define-method digest-final! ((self <blake3>) :optional (length 32))
  (%blake3-hasher-finalize (slot-ref self'context) length))

(define-method digest ((class <blake3-meta>) :optional (length 32))
  (let* ((ctx (make <blake3-context>))
         (bufsiz (* 64 1024))
         (buf (make-u8vector bufsiz)))
    (%blake3-hasher-init ctx)
    (let loop ()
      (let ((len (read-uvector! buf)))
        (cond ((eof-object? len)
               (%blake3-hasher-finalize ctx length))
              ((< len bufsiz)
               (%blake3-hasher-update ctx (uvector-alias <u8vector> buf 0 len))
               (loop))
              (else
               (%blake3-hasher-update ctx buf)
               (loop)))))))

(inline-stub
 (declcode
  (.include <blake3.h>)

  (.define LIBGAUCHE_EXT_BODY)
  (.include <gauche/extend.h>))

 (define-ctype ScmBlakeContext::(.struct
                                 (SCM_HEADER :: ""
                                             ctx::blake3_hasher)))

 (define-cclass <blake3-context> :private
   ScmBlakeContext* "Scm_BlakeContextClass" ()
   ()
   [allocator
    (let* ([ctx :: ScmBlakeContext* (SCM_NEW_INSTANCE ScmBlakeContext klass)])
      (cast void initargs)              ; suppress unused var warning
      (return (SCM_OBJ ctx)))])

 (define-cproc %blake3-hasher-init (ctx::<blake3-context>) ::<void>
   (blake3_hasher_init (& (-> ctx ctx))))

 (define-cproc %blake3-hasher-init-keyed (ctx::<blake3-context> key) ::<void>
   (unless (and (SCM_U8VECTORP key)
                (== (SCM_U8VECTOR_SIZE (SCM_U8VECTOR key)) BLAKE3_KEY_LEN))
     (Scm_Error "invalid key"))
   (blake3_hasher_init_keyed (& (-> ctx ctx))
                             (SCM_UVECTOR_ELEMENTS (SCM_U8VECTOR key))))

 (define-cproc %blake3-hasher-init-derive-key-raw (ctx::<blake3-context> context) ::<void>
   (unless (SCM_STRINGP context)
     (Scm_Error "invalid context"))
   (let* ([b::(const ScmStringBody*) (SCM_STRING_BODY context)]
          [size::size_t (SCM_STRING_BODY_SIZE b)])
     (blake3_hasher_init_derive_key_raw (& (-> ctx ctx))
                                        (SCM_STRING_BODY_START b)
                                        size)))

 (define-cproc %blake3-hasher-update (ctx::<blake3-context> data) ::<void>
   (blake3_hasher_update (& (-> ctx ctx))
                         (cast (const unsigned char*)
                               (SCM_UVECTOR_ELEMENTS (SCM_U8VECTOR data)))
                         (SCM_U8VECTOR_SIZE (SCM_U8VECTOR data))))

  (define-cproc %blake3-hasher-finalize (ctx::<blake3-context> length)
    (unless (SCM_UINTP length)
      (SCM_TYPE_ERROR length "positive <fixnum>"))
    (let* ([len::size_t (SCM_INT_VALUE length)]
           [digest::(.array (unsigned char) (len))])
      (blake3_hasher_finalize (& (-> ctx ctx)) digest len)
      (return (Scm_MakeString (cast (const char*) digest) len len
                              (logior SCM_STRING_INCOMPLETE
                                      SCM_STRING_COPYING)))))

 )
  
#|
gosh tools/precomp -e teststub.scm
gcc -fPIC -shared -o teststub.so teststub.c -I /usr/lib/gauche-0.98/0.9.15/include -I ~/src/b3sum-c ~/src/b3sum-c/libblake3.a
gosh -I. testteststub.scm
|#
