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
    (%blake3-hasher-init ctx)
    (slot-set! self 'context ctx)))

(define-method digest-update! ((self <blake3>) data)
  (%blake3-hasher-update
   (slot-ref self'context) 
   (typecase data
             (<u8vector> data)
             (<string> (string->u8vector data))
             (else (error "cannot hash this:" data)))))

(define-method digest-final! ((self <blake3>))
  (%blake3-hasher-finalize (slot-ref self'context)))

(define-method digest ((class <blake3-meta>))
  (let* ((ctx (make <blake3-context>))
         (bufsiz (* 64 1024))
         (buf (make-u8vector bufsiz)))
    (%blake3-hasher-init ctx)
    (let loop ()
      (let ((len (read-uvector! buf)))
        (cond ((eof-object? len)
               (%blake3-hasher-finalize ctx))
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
 
 (define-cproc %blake3-hasher-update (ctx::<blake3-context> data) ::<void>
   (blake3_hasher_update (& (-> ctx ctx))
                         (cast (const unsigned char*)
                               (SCM_UVECTOR_ELEMENTS (SCM_U8VECTOR data)))
                         (SCM_U8VECTOR_SIZE (SCM_U8VECTOR data))))

  (define-cproc %blake3-hasher-finalize (ctx::<blake3-context>)
    (let* ([digest::(.array (unsigned char) (BLAKE3_OUT_LEN))])
      (blake3_hasher_finalize (& (-> ctx ctx)) digest BLAKE3_OUT_LEN)
      (return (Scm_MakeString (cast (const char*) digest)
                              BLAKE3_OUT_LEN BLAKE3_OUT_LEN
                              (logior SCM_STRING_INCOMPLETE
                                      SCM_STRING_COPYING)))))

 )
  
#|
gosh tools/precomp -e teststub.scm
gcc -fPIC -shared -o teststub.so teststub.c -I /usr/lib/gauche-0.98/0.9.15/include -I ~/src/b3sum-c ~/src/b3sum-c/libblake3.a
gosh -I. testteststub.scm
|#
