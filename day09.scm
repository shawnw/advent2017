;;; $ kawa day09.scm < day09.txt
;;; $ csi -R r7rs -S day09.scm < day09.txt

(import (srfi 1))
(define (count-groups str)
  (let ((results
         (string-fold
          (lambda (c acc)
            (if (fourth acc)
                (cond ; Garbage cases
                 ((fifth acc) ; char after a !
                  (list (first acc) (second acc) (third acc) #t #f))
                 ((char=? c #\>)
                  (list (first acc) (second acc) (third acc) #f))
                 ((char=? c #\!)
                  (list (first acc) (second acc) (third acc) #t #t))
                 (else
                  (cons* (first acc) (second acc) (+ 1 (third acc))
                         (cdddr acc))))
                (cond ; Regular cases
                 ((char=? c #\<)
                  (list (first acc) (second acc) (third acc) #t #f))
                 ((char=? c #\{)
                  (cons* (+ (first acc) 1 (second acc)) (+ 1 (second acc))
                         (cddr acc)))
                 ((char=? c #\})
                  (cons* (first acc) (- (second acc) 1) (cddr acc)))
                 ((char=? c #\,)
                  acc))))
          (list 0 0 0 #f) str)))
    (values (first results) (third results))))

(receive (part1 part2) (count-groups (read-line))
         (format #t "Part 1: ~A~%Part 2: ~A~%" part1 part2))

