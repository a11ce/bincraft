#lang racket/base


(require racket/list
         racket/generator)

(provide (struct-out byte)
         x
         number->byte hexstr->bytes string->bytes bytes->string
         split-blocks len : repeat-to padding pad-before pad-after)

(struct byte (val)
  #:transparent
  #:property prop:custom-write
  (λ (b port mode)
    (display (string-upcase (string-append 
                             (number->string (quotient (byte-val b) 16) 16)
                             (number->string (remainder (byte-val b) 16) 16)))
             port)))

;;; auto conversion

(define (x dat)
  (cond
    [(byte? dat) dat]
    [(string? dat) (string->bytes dat)]
    [(list? dat) (map x dat)]
    [(and (number? dat) (<= 0 dat 255)) (byte dat)]
    [(equal? #f dat) '()]
    [else (raise-arguments-error
           'x
           "unknown type to convert"
           "given" dat)]))

;;; special conversions

(define (number->byte n)
  (byte n))

(define (hexstr->bytes h)
  (map (λ (b) (number->byte
               (string->number (format "#x~a"
                                       (list->string b)))))
       (split-blocks (string->list h) 2)))

(define (string->bytes str)
  (map (compose number->byte
                char->integer)
       (string->list str)))

(define (bytes->string dat #:split [split-size #f])
  (define chars
    (map (compose integer->char byte-val) (x dat)))
  (if split-size
      (map list->string (split-blocks chars split-size))
      (list->string chars)))

;;; craft utils

(define (split-blocks lst [blocksize 16])
  (cond
    [(null? lst) '()]
    [(< (length lst) blocksize) (list lst)]
    [else
     (cons (take lst blocksize)
           (split-blocks (drop lst blocksize) blocksize))]))

(define (len dat)
  (length (x dat)))

(define (: . args)
  (flatten (map x (filter values args))))

(define (repeat-to dat size)
  (define gen (sequence->repeated-generator dat))
  (for/list ([_ (in-range size)])
    (gen)))

(define (padding size)
  (repeat-to (x "meow")
             size))

(define (pad-before dat size)
  (: (padding (- size (len dat)))
     dat))

(define (pad-after dat size)
  (: dat
     (padding (- size (len dat)))))