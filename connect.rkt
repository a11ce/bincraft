#lang racket/base

(require racket/tcp)

(provide connect
         send
         sendln
         recv-line
         disconnect)

(struct connection (in out))

(define (connect host port)
  (define-values (in out) (tcp-connect host port))
  (connection in out))

(define (recv-line conn [multi #f])
  (if multi
      (for/list ([_ (in-range multi)])
        (read-line (connection-in conn)))
      (read-line (connection-in conn))))

(define (sendln conn dat)
  (send conn (string-append dat "\n"))
  (flush-output (connection-out conn)))

(define (send conn dat)
  (write-string dat (connection-out conn)))

(define (disconnect conn)
  (close-input-port (connection-in conn))
  (close-output-port (connection-out conn)))