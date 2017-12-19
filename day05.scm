(import (io-utils))
(define-syntax until-out-of-bounds
  (syntax-rules (on-bounds-error)
    ((_ body (on-bounds-error errbody))
     (try-catch body (e java.lang.ArrayIndexOutOfBoundsException errbody)))))

(define (step-through jumps #!optional (offset3 :: int 1))
  (let ((jumps (list->s32vector jumps))
        (steps ::int 0))
    (until-out-of-bounds
     (let loop ((i ::int 0))
       (let ((v (s32vector-ref jumps i)))
         (if (>= v 3)
             (s32vector-set! jumps i (+ v offset3))
             (s32vector-set! jumps i (+ v 1)))
         (set! steps (+ steps 1))
         (loop (+ i v))))
     (on-bounds-error steps))))

(define input (read-numbers))
(define test-input '(0 3 0 1 -3))
(format #t "Test 1: ~A~%" (step-through test-input))
(format #t "Part 1: ~A~%" (step-through input))
(format #t "Test 2: ~A~%" (step-through test-input -1))
(format #t "Part 2: ~A~%" (step-through input -1))

            
