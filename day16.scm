(import (srfi 1) (srfi 133) (kawa regex) (rnrs hashtables))

(define (parse-input input)
  (let ((s-re (regex "^s(\\d+)$"))
        (x-re (regex "^x(\\d+)/(\\d+)$"))
        (p-re (regex "^p([a-z])/([a-z])$")))
    (map (lambda (atom)
           (cond
            ((regex-match s-re atom) =>
             (lambda (bits)
               (cons 'spin (string->number (second bits)))))
            ((regex-match x-re atom) =>
             (lambda (bits)
               (cons 'exchange
                     (cons (string->number (second bits))
                           (string->number (third bits))))))
            ((regex-match p-re atom) =>
             (lambda (bits)
               (cons 'partner (cons (string-ref (second bits) 0)
                                    (string-ref (third bits) 0)))))
            (else
             (error "Invalid input" atom)))) input)))

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

(define (solve-part1! programs input)
  (for-each (lambda (move)
              (case (car move)
                ((spin)
                 (set! programs (spin programs (cdr move))))
                ((exchange)
                 (exchange! programs (cadr move) (cddr move)))
                ((partner)
                 (partner! programs (cadr move) (cddr move)))))
            input)
  programs)

(define (solve-part2 input)
  (let* ((programs (string->vector "abcdefghijklmnop"))
         (seen (make-hashtable string-hash string=? 100))
         (seen2 (make-hashtable (lambda (x) x) = 100))
         (billion ::int 1000000000))
    (hashtable-set! seen (vector->string programs) 0)
    (hashtable-set! seen2 0 (vector->string programs))
    (let loop ((i ::int 1)
               (programs (solve-part1! programs input)))
      (if (= i billion)
          programs
          (let* ((as-string (vector->string programs))
                 (cached (hashtable-ref seen as-string #f)))
            (if cached
                (let* ((diff (- i cached))
                       (offset (remainder billion diff)))
                  (hashtable-ref seen2 offset #f))
                (begin
                  (hashtable-set! seen as-string i)
                  (hashtable-set! seen2 i as-string)
                  (loop (+ i 1) (solve-part1! programs input)))))))))

(define input (parse-input (string-split (read-line) ",")))
(format #t "Part 1: ~A~%" (vector->string
                           (solve-part1! (string->vector "abcdefghijklmnop")
                                        input)))
(format #t "Part 2: ~A~%" (solve-part2 input))
