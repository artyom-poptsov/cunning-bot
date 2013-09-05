(define-module (cunning-bot plugins debug)
  #:use-module (ice-9 match)
  #:export (debug))

(define (set-debugging! val)
  (set! (@@ (cunning-bot bot) debugging) val))

(define (toggle-debugging!)
  (set! (@@ (cunning-bot bot) debugging)
        (not (@@ (cunning-bot bot) debugging))))

(define (debug sender args)
  (match (string-downcase (string-trim-both args))
    ("on" (set-debugging! #t) "done")
    ("off" (set-debugging! #f) "done")
    ("" (toggle-debugging!) "toggled")
    (other (string-append "option not supported: " other))))