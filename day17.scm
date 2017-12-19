(cond-expand
 (kawa
  (import (srfi 1)))
 (chicken
  (require-extension srfi-1)
  (require-extension format)
  (declare (fixnum))))

(define (spin steps reps goal)
  (let* ((buffer (circular-list 0))
         (starting-point buffer))
    (do ((i 1 (+ i 1)))
        ((> i reps) (cadr (member goal starting-point =)))
      (let* ((point (drop buffer steps))
             (new (cons i (cdr point))))
        (when (= (remainder i 1000000) 0)
              (write-char (if (= (remainder i 5000000) 0) #\+ #\.))
              (flush-output))
        (set-cdr! point new)
        (set! buffer new)))))

(format #t "Test 1: ~A~%" (spin 3 2017 2017))
(format #t "Part 1: ~A~%" (spin 344 2017 2017))
(format #t "~%Part 2: ~A~%" (spin 344 50000000 0))



