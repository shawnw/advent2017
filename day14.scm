(import (srfi 1) (list-utils) (knot-hash))

(define (make-input seed row) (format "~A-~A" seed row))

(define (to-bits n) (format "~4,'0B" n))
(define (as-bits lst) (string-concatenate (map to-bits lst)))

(define (grid-ref grid::vector x::int y::int)
  (vector-ref (vector-ref grid y) x))
                  
(define (grid-set! grid::vector x::int y::int val)
  (vector-set! (vector-ref grid y) x val))

(define (fill-region! grid::vector x::int y::int c)
  (grid-set! grid x y c)
  (if (and (> x 0) (eq? (grid-ref grid (- x 1) y) #t))
      (fill-region! grid (- x 1) y c))
  (if (and (< x 127) (eq? (grid-ref grid (+ x 1) y) #t))
      (fill-region! grid (+ x 1) y c))
  (if (and (> y 0) (eq? (grid-ref grid x (- y 1)) #t))
      (fill-region! grid x (- y 1) c))
  (if (and (< y 127) (eq? (grid-ref grid x (+ y 1)) #t))
      (fill-region! grid x (+ y 1) c)))
  
(define (count-regions grid)
  (let ((grid (list->vector
                (map (lambda (line)
                       (string->vectormap
                        (lambda (c)
                          (if (char=? c #\1) #t #f))
                        (as-bits line)))
                     grid)))
         (regions ::int 0))
    (do ((x ::int 0 (+ x 1)))
        ((= x 128) regions)
      (do ((y ::int 0 (+ y 1)))
          ((= y 128))
        (when (eq? (grid-ref grid x y) #t)
              (set! regions (+ regions 1))
              (fill-region! grid x y regions))))))

(define c2n (map (lambda (c)
                   (cons c (string->number (string c) 16)))
                 (string->list "0123456789abcdef")))

(define (solve seed)
  (let loop ((n 0)
             (bits 0)
             (grid '()))
    (if (= n 128)
        (values bits (count-regions grid))
        (let* ((hashed (knot-hash (make-input seed n)))
               (asnums (map (lambda (hexdigit)
                              (cdr (assq hexdigit c2n)))
                            (string->list hashed))))
          (loop (+ n 1)
                (+ bits (fold (lambda (n bits) (+ bits (bitwise-bit-count n)))
                              0 asnums))
                (cons asnums grid))))))

(format #t "Test 1: ~A~%" (solve "flqrgnkx"))
(format #t "Parts 1 & 2: ~A~%" (solve "ffayrhll"))

        
