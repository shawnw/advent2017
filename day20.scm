(import (srfi 1) (kawa regex) (kawa quaternions) (io-utils)
        (rnrs hashtables) (only (srfi 69) hash))

(define particle-re (regex "^p=<(-?\\d+),(-?\\d+),(-?\\d+)>,\\s+v=<(-?\\d+),(-?\\d+),(-?\\d+)>,\\s+a=<(-?\\d+),(-?\\d+),(-?\\d+)>\\s*$"))

(define (make-particle px py pz sx sy sz ax ay az)
  (vector (make-vector-quaternion px py pz)
          (make-vector-quaternion sx sy sz)
          (make-vector-quaternion ax ay az)))

(define (read-particles lines)
  (map (lambda (nums) (apply make-particle (map string->number nums)))
       (map (lambda (line) (cdr (regex-match particle-re line))) lines)))

(define (update-particle part)
  (let* ((accel (vector-ref part 2))
         (velo (+ (vector-ref part 1) accel))
         (pos (+ (vector-ref part 0) velo)))
    (vector pos velo accel)))

(define (tick particles)
  (map update-particle particles))

(define (distance-from-zero part)
  (let ((pos (vector-ref part 0)))
    (+ (abs (imag-part pos)) (abs (jmag-part pos)) (abs (kmag-part pos)))))

(define (find-closest particles)
  (second (fold (lambda (part acc)
                  (let* ((distance (distance-from-zero part))
                         (curr-elt (third acc))
                         (next-elt (+ curr-elt 1)))
                    (if (< distance (first acc))
                        (list distance curr-elt next-elt)
                        (list (first acc) (second acc) next-elt))))
                (list (distance-from-zero (car particles)) 0 1) (cdr particles))))

(define (particle=? a b)
  (= (vector-ref a 0) (vector-ref b 0)))
(define (quat-hash q)
  (hash (+ (imag-part q) (jmag-part q) (kmag-part q))))

(define (delete-collisions particles)
  (let ((seen (make-hashtable quat-hash =)))
    (for-each
     (lambda (particle)
       (if (hashtable-contains? seen (vector-ref particle 0))
           (set! particles (remove (cut particle=? particle <>) particles))
           (hashtable-set! seen (vector-ref particle 0) #t)))
     particles)
    particles))

(define (simulate particles reps prune?)
  (let ((run-tick (if prune?
                      (lambda (particles) (delete-collisions (tick particles)))
                      tick)))
    (do ((i 1 (+ i 1))
         (parts (run-tick particles) (run-tick parts)))
        ((= i reps) parts))))

(define (look-for-closest particles reps)
  (let* ((parts2 (simulate particles reps #f))
         (parts3 (simulate parts2 (+ reps 10) #f))
         (parts4 (simulate parts3 (+ reps 25) #f))
         (closest1 (find-closest particles))
         (closest2 (find-closest parts2))
         (closest3 (find-closest parts3))
         (closest4 (find-closest parts4)))
    (if (= closest1 closest2 closest3 closest4)
        closest1
        (look-for-closest parts3 (+ reps 40)))))

(define (look-for-collisions particles)
  (let* ((parts2 (simulate particles 10 #t))
         (parts3 (simulate parts2 20 #t))
         (parts4 (simulate parts3 30 #t))
         (len1 (length particles))
         (len2 (length parts2))
         (len3 (length parts3))
         (len4 (length parts4)))
    (if (= len1 len2 len3 len4)
        len1
        (look-for-collisions parts4))))

(define test1-particles (read-particles '("p=<3,0,0>, v=<2,0,0>, a=<-1,0,0>"
                                     "p=<4,0,0>, v=<0,0,0>, a=<-2,0,0>")))
(define test2-particles (read-particles '("p=<-6,0,0>, v=<3,0,0>, a=<0,0,0>"
                                          "p=<-4,0,0>, v=<2,0,0>, a=<0,0,0>"
                                          "p=<-2,0,0>, v=<1,0,0>, a=<0,0,0>"
                                          "p=<3,0,0>, v=<-1,0,0>, a=<0,0,0>")))
(define input-particles (read-particles (read-lines)))

(format #t "Test 1: ~A~%" (look-for-closest test1-particles 1))
(format #t "Part 1: ~A~%" (look-for-closest input-particles 10))
(format #t "Test 2: ~A~%" (look-for-collisions test2-particles))
(format #t "Part 2: ~A~%" (look-for-collisions input-particles))
