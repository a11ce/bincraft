#lang racket/base

(require bincraft/bin)

(provide printable-ascii lockpick)

(define printable-ascii
  (: "abcdefghijklmnopqrstuvwxyz"
     "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
     "0123456789"
     " !\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~"))

(define (lockpick p size choices [init-acc #f])
  (for/fold ([acc init-acc])
            ([idx (in-range 1 (add1 size))])
    (displayln (format "~a/~a..." idx size))
    (: acc
       (for/first ([c choices]
                   #:when (p c acc))
         c))))