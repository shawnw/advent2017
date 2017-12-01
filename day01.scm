;;; kawa day01.scm < day01.txt
;;; or
;;; csi -s day01.scm < day01.txt
;;; or
;;; guile -s day01.scm < day01.txt
(cond-expand
  (guile
   (use-modules (srfi srfi-11)
                (ice-9 rdelim)))
   (else))
;;; Only kawa has a string->vector, which combined with R6RS vector-map
;;; would make this more efficient. Why am I supporting multiple schemes?
(define (transform str)
  (list->vector (map (lambda (c) (- (char->integer c) (char->integer #\0)))
              (string->list str))))
(define (add-if= a b sum) (if (= a b) (+ a sum) sum))
(define (captcha vec)
  (let* ((len (vector-length vec))
         (half (quotient len 2))
         (get-next-elem (lambda (n step)
                          (vector-ref vec (remainder (+ step n) len)))))
    (let loop ((pos 0)
               (sum1 0)
               (sum2 0))
      (if (= pos len)
          (values sum1 sum2)
          (loop (+ pos 1)
                (add-if= (vector-ref vec pos) (get-next-elem pos 1) sum1)
                (add-if= (vector-ref vec pos) (get-next-elem pos half) sum2))))))

(display "Tests (part 1):")
(newline)
(for-each (lambda (input)
            (let-values (((part1 part2) (captcha (transform input))))
              (format #t "~A: ~A~%" input part1)))
          '("1122" "1111" "1234" "91212129"))
(display "Tests (part 2):\n")
(for-each (lambda (input)
            (let-values (((part1 part2) (captcha (transform input))))
              (format #t "~A: ~A~%" input part2)))
          '("1212" "1221" "123425" "123123" "12131415"))

(let-values (((part1 part2) (captcha (transform (read-line)))))
  (format #t "Part 1: ~A~%Part 2: ~A~%" part1 part2))
