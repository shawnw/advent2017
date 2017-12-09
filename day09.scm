(import (srfi 1))
(define (count-groups str)
  (let ((results
         (string-fold
          (lambda (c acc)
            (cond
             ((and (fourth acc) (fifth acc)) ; char after a !
              (list (first acc) (second acc) (third acc) #t #f))
             ((and (fourth acc) (char=? c #\>))
              (list (first acc) (second acc) (third acc) #f #f))
             ((and (fourth acc) (char=? c #\!))
              (list (first acc) (second acc) (third acc) #t #t))
             ((fourth acc)
              (list (first acc) (second acc) (+ 1 (third acc)) #t #f))
             ((char=? c #\<)
              (list (first acc) (second acc) (third acc) #t #f))
             ((char=? c #\{)
              (list (+ (first acc) 1 (second acc)) (+ 1 (second acc)) (third acc) #f #f))
             ((char=? c #\})
              (list (first acc) (- (second acc) 1) (third acc) #f #f))
             ((char=? c #\,)
              acc)))
          (list 0 0 0 #f #f) str)))
    (values (first results) (third results))))

(format #t "Part 1 and 2: ~A~%" (count-groups (read-line)))

