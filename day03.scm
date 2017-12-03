(import (rnrs hashtables))
(define input 361527)
(define (taxicab p1 p2)
  (+ (abs (- (car p2) (car p1))) (abs (- (cdr p2) (cdr p1)))))

(define (left p) (cons (- (car p) 1) (cdr p)))
(define (right p) (cons (+ (car p) 1) (cdr p)))
(define (up p) (cons (car p) (+ (cdr p) 1)))
(define (down p) (cons (car p) (- (cdr p) 1)))

(define origin (cons 0 0))
(define grid (make-hashtable equal-hash equal? input))
(define rgrid (make-hashtable (lambda (n) n) = input))
(define (add-to-grid! n loc #!optional (v #f))
  (hashtable-set! grid loc v)
  (hashtable-set! rgrid n loc))
(add-to-grid! 1 origin 1)
(add-to-grid! 2 (right origin) 1)

(define (insert n)
  (let* ((prev (hashtable-ref rgrid (- n 1) #f))
         (u (up prev))
         (d (down prev))
         (r (right prev))
         (l (left prev)))
    (cond
     ((and (not (hashtable-contains? grid r))
           (hashtable-contains? grid u))
      (add-to-grid! n r))
     ((and (not (hashtable-contains? grid l))
           (hashtable-contains? grid d))
      (add-to-grid! n l))
     ((and (not (hashtable-contains? grid r))
           (not (hashtable-contains? grid u))
           (hashtable-contains? grid l))
      (add-to-grid! n u))
     ((and (not (hashtable-contains? grid l))
           (not (hashtable-contains? grid d))
           (hashtable-contains? grid r))
      (add-to-grid! n d))
     (else
      (error "Invalid grid state!"))))
  (let* ((loc (hashtable-ref rgrid n #f))
         (c (lambda (loc) (hashtable-ref grid loc 0)))
         (u (up loc))
         (d (down loc))
         (r (right loc))
         (l (left loc)))
    (hashtable-set! grid loc
                    (+
                     (c u)
                     (c (right u))
                     (c r)
                     (c (down r))
                     (c d)
                     (c (left d))
                     (c l)
                     (c (up l))))))

(define (add-to-grid n)
  (let loop ((x 1))
    (when (<= x n)
          (if (not (hashtable-contains? rgrid x))
              (insert x))
          (loop (+ x 1)))))

(define (get-count n)
  (hashtable-ref grid (hashtable-ref rgrid n #f) #f))

;;; Tests
(add-to-grid input)
(format #t "Test 1: Distance ~A~%" (taxicab origin (hashtable-ref rgrid 12 #f)))
(format #t "Test 2: Distance ~A~%" (taxicab origin (hashtable-ref rgrid 23 #f)))
(format #t "Test 3: Distance ~A~%" (taxicab origin (hashtable-ref rgrid 1024 #f)))
(format #t "Part 1: Distance ~A~%" (taxicab origin (hashtable-ref rgrid input #f)))

(format #t "Test 4: Square 2 count ~A~%" (get-count 2))
(format #t "Test 5: Square 3 count ~A~%" (get-count 3))
(format #t "Test 6: Square 4 count ~A~%" (get-count 4))
(format #t "Test 7: Square 5 count ~A~%" (get-count 5))

(let loop ((n 2))
  (let ((v (get-count n)))
    (if (> v input)
        (format #t "Part 2: ~A~%" v)
        (loop (+ n 1)))))






