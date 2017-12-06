(import (srfi 126) (io-utils))

(define (max-index vec)
  (let ((len (vector-length vec)))
    (let loop ((i 1)
               (maxi 0)
               (maxv (vector-ref vec 0)))
      (if (= i len)
          maxi
          (let ((thisv (vector-ref vec i)))
            (if (> thisv maxv)
                (loop (+ i 1) i thisv)
                (loop (+ i 1) maxi maxv)))))))

(define (vector-add! vec n)
  (do ((i (- (vector-length vec) 1) (- i 1)))
      ((< i 0))
    (vector-set! vec i (+ (vector-ref vec i) n))))

(define (distribute! vec i)
  (let* ((val (vector-ref vec i))
         (len (vector-length vec))
         (modincr (lambda (n) (mod (+ n 1) len))))
    (vector-set! vec i 0)
    (let loop ((n (modincr i))
               (val val))
      (cond
       ((= val 0))
       ((>= val len)
        (vector-add! vec 1)
        (loop n (- val len)))
       (else
        (vector-set! vec n (+ (vector-ref vec n) 1))
        (loop (modincr n) (- val 1)))))))

(define (count-cycles vec)
  (let ((seen (make-hashtable equal-hash equal?))
        (vec (vector-copy vec)))
    (hashtable-set! seen vec 0)
    (let loop ((cycles 1))
      (distribute! vec (max-index vec))
      (let-values (((occurred found) (hashtable-lookup seen vec)))
        (if found
          (values cycles (- cycles occurred))
          (begin
            (hashtable-set! seen (vector-copy vec) cycles)
            (loop (+ cycles 1))))))))

(define input (list->vector (read-numbers)))
(define test-input (vector 0 2 7 0))
(format #t "Test 1: ~A~%" (count-cycles test-input))
(format #t "Part 1 & 2: ~A~%" (count-cycles input))
