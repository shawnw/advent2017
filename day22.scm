(import (rnrs hashtables)
        (only (srfi 69) hash)
        (io-utils))

(define-record-type virus (make-virus location direction)
  virus?
  (location get-location set-location!)
  (direction get-direction set-direction!))

(define (read-map input)
  (let ((grid (make-hashtable (lambda (v::complex)
                                (abs (+ (real-part v) (hash (imag-part v)))))
                              =))
        (width (quotient (string-length (vector-ref input 0)) 2))
        (height (quotient (vector-length input) 2)))
    (do ((row (- 0 height) (+ row 1)))
        ((> row height) grid)
      (do ((col (- 0 width) (+ col 1)))
          ((> col width))
        (hashtable-set! grid (+ row (* col 1i))
                        (if (char=?
                             (string-ref (vector-ref input (+ row height)) (+ col width))
                             #\#)
                            'infected
                            'clean))))))

(define (get-node grid loc)
  (hashtable-ref grid loc 'clean))

(define (grid-set! grid loc mode)
  (hashtable-set! grid loc mode))
(define (turn-right! virus)
  (set-direction! virus
                  (case (get-direction virus)
                    ((up) 'right)
                    ((right) 'down)
                    ((down) 'left)
                    ((left) 'up))))

(define (turn-left! virus)
  (set-direction! virus
                  (case (get-direction virus)
                    ((up) 'left)
                    ((left) 'down)
                    ((down) 'right)
                    ((right) 'up))))

(define (move-forward! virus)
  (let ((loc ::complex (get-location virus)))
    (case (get-direction virus)
      ((up) (set-location! virus (- loc 1)))
      ((left) (set-location! virus (- loc 1i)))
      ((down) (set-location! virus (+ loc 1)))
      ((right) (set-location! virus (+ loc 1i))))))

(define (burst! grid virus) ::int
  (let ((loc (get-location virus)))
    (if (eq? (get-node grid loc) 'infected)
        (begin
          (turn-right! virus)
          (grid-set! grid loc 'clean)
          (move-forward! virus)
          0)
        (begin
          (turn-left! virus)
          (grid-set! grid loc 'infected)
          (move-forward! virus)
          1))))

(define (burst2! grid virus) ::int
  (let ((loc (get-location virus)))
    (case (get-node grid loc)
      ((clean)
       (turn-left! virus)
       (grid-set! grid loc 'weakened)
       (move-forward! virus)
       0)
      ((weakened)
       (grid-set! grid loc 'infected)
       (move-forward! virus)
       1)
      ((infected)
       (grid-set! grid loc 'flagged)
       (turn-right! virus)
       (move-forward! virus)
       0)
      ((flagged)
       (grid-set! grid loc 'clean)
       (turn-left! virus)
       (turn-left! virus)
       (move-forward! virus)
       0))))

(define (activity! grid reps::int part2?)
  (let ((virus (make-virus 0 'up))
        (do-burst! (if part2? burst2! burst!)))
    (do ((i ::int 0 (+ i 1))
         (infected-count ::int 0 (+ infected-count (do-burst! grid virus))))
        ((= i reps) infected-count))))

(define (display-grid grid virus width height)
  (let ((width (quotient width 2))
        (height (quotient height 2))
        (vloc (get-location virus)))
    (do ((row (- 0 height) (+ row 1)))
        ((> row height))
      (do ((col (- 0 width) (+ col 1)))
          ((> col width) (newline))
        (let ((loc (+ row (* col 1i))))
          (if (= vloc loc)
              (write-char (if (eq? (get-node grid loc) 'infected) #\& #\:))
              (write-char (if (eq? (get-node grid loc) 'infected) #\# #\.))))))))

(define test-map '#("..#" "#.." "..."))
(format #t "Test 1: ~A~%" (activity! (read-map test-map) 7 #f))
(format #t "Test 2: ~A~%" (activity! (read-map test-map) 70 #f))
(format #t "Test 3: ~A~%" (activity! (read-map test-map) 10000 #f))

(define real-map (list->vector (read-lines)))
(format #t "Part 1: ~A~%" (activity! (read-map real-map) 10000 #f))
(format #t "Test 4: ~A~%" (activity! (read-map test-map) 100 #t))
(format #t "Part 2: ~A~%" (activity! (read-map real-map) 10000000 #t))





