;;; csc -o day05 -optimize-level 3 day05-chicken.scm
(declare (fixnum-arithmetic)
         (strict-types)
         (block)
         (specialize)
         (disable-interrupts))
(require-extension srfi-4)
(require-extension ports)

(define (read-numbers)
  (port-map
   (lambda (i)
     (if (not (number? i))
         (error "Invalid input" i)
         i))
   read))

(define-syntax until-out-of-bounds
  (syntax-rules (on-bounds-error)
    ((_ body (on-bounds-error errbody))
     (condition-case body (e (exn bounds) errbody)))))

(define (step-through jumps #!optional (offset3 1))
  (let ((jumps (list->s32vector jumps))
        (steps 0)
        (offset3 (the fixnum offset3)))
    (until-out-of-bounds
     (let loop ((i 0))
       (let ((v (the fixnum (s32vector-ref jumps i))))
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
