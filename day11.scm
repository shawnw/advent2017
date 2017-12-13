(import (srfi 1) (kawa quaternions))

(define north      0i+1j-1k)
(define northeast  1i+0j-1k)
(define southeast  1i-1j+0k)
(define south      0i-1j+1k)
(define southwest -1i+0j+1k)
(define northwest -1i+1j+0k)
(define directions `((n . ,north) (ne . ,northeast) (se . ,southeast)
                     (s . ,south) (sw . ,southwest) (nw . ,northwest)))

(define (distance p1::quaternion p2::quaternion)
  (let ((d (- p1 p2)))
    (max (abs (imag-part d))
         (abs (jmag-part d))
         (abs (kmag-part d)))))

(define (find-distance route)
  (let* ((origin 0)
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
               
          


        
