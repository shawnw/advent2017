(import (srfi 1) (srfi 133) (kawa regex) (rnrs hashtables))

(define (spin programs x)
  (let ((len (vector-length programs)))
    (vector-append-subvectors programs (- len x) len
                              programs 0 (- len x))))

(define (exchange! programs a b)
  (vector-swap! programs a b))

(define (partner! programs a b)
  (let ((posa (vector-index (cut char=? a <>) programs))
        (posb (vector-index (cut char=? b <>) programs)))
    (vector-swap! programs posa posb)))

(define s-re (regex "^s(\\d+)$"))
(define x-re (regex "^x(\\d+)/(\\d+)$"))
(define p-re (regex "^p([a-z])/([a-z])$"))

(define (solve-part1! programs input)
  (for-each (lambda (direction)
              (cond
               ((regex-match s-re direction) =>
                (lambda (bits)
                  (set! programs
                        (spin programs (string->number (second bits))))))
               ((regex-match x-re direction) =>
                (lambda (bits)
                  (exchange! programs (string->number (second bits))
                             (string->number (third bits)))))             
               ((regex-match p-re direction) =>
                (lambda (bits)
                  (partner! programs (string-ref (second bits) 0)
                            (string-ref (third bits) 0))))
               (else
                (error "Unknown direction" direction))))
            input)
  programs)

(define (solve-part2 input)
  (let* ((programs (string->vector "abcdefghijklmnop"))
         (seen (make-hashtable string-hash string=?))
         (seen2 (make-hashtable (lambda (x) x) =))
         (billion 1000000000))
    (hashtable-set! seen (vector->string programs) 0)
    (hashtable-set! seen2 0 (vector->string programs))
    (let loop ((i 1))
      (if (= i billion)
          programs
          (begin
            (set! programs (solve-part1! programs input))
            (let* ((as-string (vector->string programs))
                   (cached (hashtable-ref seen as-string #f)))
              (if cached
                  (let* ((diff (- i cached))
                         (left (ceiling (/ (- billion i) diff)))
                         (newi (* diff left)))
                    (hashtable-ref seen2 (- billion newi) #f))
                  (begin
                    (hashtable-set! seen as-string i)
                    (hashtable-set! seen2 i as-string)
                    (loop (+ i 1))))))))))

(define input (string-split (read-line) ","))
(format #t "Part 1: ~A~%" (vector->string
                           (solve-part1! (string->vector "abcdefghijklmnop")
                                        input)))
(format #t "Part 2: ~A~%" (solve-part2 input))
