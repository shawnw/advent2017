(import (srfi 133) (io-utils))

(define grid (list->vector (map string->vector (read-lines))))

(define max-x (vector-length (vector-ref grid 0)))
(define max-y (vector-length grid))

(define letters '())
(define steps 0)

(define (grid-ref x y)
  (vector-ref (vector-ref grid y) x))

(define (in-bounds? x y)
  (and (>= x 0) (< x max-x)
       (>= y 0) (< y max-y)
       (not (char-whitespace? (grid-ref x y)))))

(define (circuit-char? c)
  (or (char=? c #\-) (char=? c #\|) (char=? c #\+) (char-upper-case? c)))

(define (move-down x y)
  (if (in-bounds? x y)
      (let ((c (grid-ref x y)))
        (set! steps (+ steps 1))
        (if (char-upper-case? c)
            (set! letters (cons c letters)))
        (if (char=? c #\+)
            (cond
             ((and (> x 0) (circuit-char? (grid-ref (- x 1) y)))
              (move-left (- x 1) y))
             ((and (< (+ x 1) max-x) (circuit-char? (grid-ref (+ x 1) y)))
              (move-right (+ x 1) y))
             (else
              (error "invalid state at" x y)))
            (move-down x (+ y 1))))
      (format #t "Exited at (~A,~A)~%" x y)))

(define (move-up x y)
    (if (in-bounds? x y)
        (let ((c (grid-ref x y)))
          (set! steps (+ steps 1))
          (if (char-upper-case? c)
              (set! letters (cons c letters)))
          (if (char=? c #\+)
              (cond
               ((and (> x 0) (circuit-char? (grid-ref (- x 1) y)))
                (move-left (- x 1) y))
               ((and (< (+ x 1) max-x) (circuit-char? (grid-ref (+ x 1) y)))
                (move-right (+ x 1) y))
               (else
                (error "invalid state at" x y)))
              (move-up x (- y 1))))
        (format #t "Exited at (~A,~A)~%" x y)))

(define (move-left x y)
    (if (in-bounds? x y)
        (let ((c (grid-ref x y)))
          (set! steps (+ steps 1))
          (if (char-upper-case? c)
              (set! letters (cons c letters)))
          (if (char=? c #\+)
              (cond
               ((and (> y 0) (circuit-char? (grid-ref x (- y 1))))
                (move-up x (- y 1)))
               ((and (< (+ y 1) max-y) (circuit-char? (grid-ref x (+ y 1))))
                (move-down x (+ y 1)))
               (else
                (error "invalid state at" x y)))
              (move-left (- x 1) y)))
        (format #t "Exited at (~A,~A)~%" x y)))

(define (move-right x y)
    (if (in-bounds? x y)
        (let ((c (grid-ref x y)))
          (set! steps (+ steps 1))
          (if (char-upper-case? c)
              (set! letters (cons c letters)))
          (if (char=? c #\+)
              (cond
               ((and (> y 0) (circuit-char? (grid-ref x (- y 1))))
                (move-up x (- y 1)))
               ((and (< (+ y 1) max-y) (circuit-char? (grid-ref x (+ y 1))))
                (move-down x (+ y 1)))
               (else
                (error "invalid state at" x y)))
              (move-right (+ x 1) y)))
        (format #t "Exited at (~A,~A)~%" x y)))

(define x (vector-index (lambda (ch) (char=? ch #\|)) (vector-ref grid 0)))
(define y 0)

(format #t "Starting at (~A,~A)~%" x y)
(move-down x y)
(format #t "Part 1: ~A~%" (reverse-list->string letters))
(format #t "Part 2: ~A~%" steps)

