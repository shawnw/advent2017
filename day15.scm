(define (rng factor::long)
  (lambda (seed::long)
    (remainder (* seed factor) java.lang.Integer:MAX_VALUE)))

(define (rng2 factor::long multiple::int)
  (let ((r (rng factor)))
    (lambda (seed::long) ::int
      (let loop ((val ::int (r seed)))
        (if (= (remainder val multiple) 0)
            val
            (loop (r val)))))))

(define a (rng 16807))
(define b (rng 48271))
(define a2 (rng2 16807 4))
(define b2 (rng2 48271 8))

(define seeda 883)
(define seedb 879)

(define (judge count::int rnga rngb)
	(let ((score ::int 0))
		(do ((rep ::int 0 (+ rep 1))
				(vala ::int (rnga seeda) (rnga vala))
				(valb ::int (rngb seedb) (rngb valb)))
			((= rep count) score)
			(if (= (bitwise-and vala #xFFFF) (bitwise-and valb #xFFFF))
				(set! score (+ score 1))))))

(format #t "Part 1: ~A~%" (judge 40000000 a b))
(format #t "Part 2: ~A~%" (judge  5000000 a2 b2))
