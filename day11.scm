(import (srfi 1) (kawa quaternions))

(define north (make-vector-quaternion 0 1 -1))
(define northeast (make-vector-quaternion 1 0 -1))
(define southeast (make-vector-quaternion 1 -1 0))
(define south (make-vector-quaternion 0 -1 1))
(define southwest (make-vector-quaternion -1 0 1))
(define northwest (make-vector-quaternion -1 1 0))
(define directions `((n . ,north) (ne . ,northeast) (se . ,southeast)
                     (s . ,south) (sw . ,southwest) (nw . ,northwest)))

(define (distance p1 p2)
  (apply max (map abs (vector-quaternion->list (- p1 p2)))))

(define (find-distance route)
  (let* ((origin (make-vector-quaternion 0 0 0))
         (destination
          (fold
           (lambda (move points)
             (let ((maxpoint (cdr points))
                   (newpoint
                    (+ (car points) (cdr (assq move directions)))))
               (cons newpoint (if (> (distance origin newpoint)
                                     (distance origin maxpoint))
                                  newpoint maxpoint))))
           (cons origin origin) route)))
    (values (distance origin (car destination))
            (distance origin (cdr destination)))))

(format #t "Test 1: ~A~%" (find-distance '(ne ne ne)))
(format #t "Test 2: ~A~%" (find-distance '(ne ne sw sw)))
(format #t "Test 3: ~A~%" (find-distance '(ne ne s s)))
(format #t "Test 4: ~A~%" (find-distance '(se sw se sw sw)))
(define input (map string->symbol (string-split (read-line) ",")))
(format #t "Part 1 and 2: ~A~%" (find-distance input))
               
          


        
