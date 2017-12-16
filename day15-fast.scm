(define (rngA seed::long)
    (remainder (* seed 16807) java.lang.Integer:MAX_VALUE))

(define (rngB seed::long)
    (remainder (* seed 48271) java.lang.Integer:MAX_VALUE))

(define (rngA2 seed::long) ::int
	(do ((val ::int (rngA seed) (rngA val)))
		((= (remainder val 4) 0) val)))

(define (rngB2 seed::long) ::int
	(do ((val ::int (rngB seed) (rngB val)))
			((= (remainder val 8) 0) val)))

(define seeda 883)
(define seedb 879)

(define (judge-part1) ::int
	(let ((score ::int 0))
    (do ((rep ::int 0 (+ rep 1))
      	 (vala ::int (rngA seeda) (rngA vala))
    		 (valb ::int (rngB seedb) (rngB valb)))
    		((= rep 40000000) score)
       (if (= (bitwise-and vala #xFFFF) (bitwise-and valb #xFFFF))
       	 (set! score (+ score 1))))))

(define (judge-part2) ::int
	(let ((score ::int 0))
    (do ((rep ::int 0 (+ rep 1))
      	 (vala ::int (rngA2 seeda) (rngA2 vala))
    		 (valb ::int (rngB2 seedb) (rngB2 valb)))
    		((= rep 5000000) score)
       (if (= (bitwise-and vala #xFFFF) (bitwise-and valb #xFFFF))
       	 (set! score (+ score 1))))))

(format #t "Part 1: ~A~%" (judge-part1))
(format #t "Part 2: ~A~%" (judge-part2))
