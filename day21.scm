(import (srfi 1) (rnrs hashtables) (only (srfi 69) hash) (uvector-utils) (kawa regex) (io-utils))

(define (hash-u8 v)
  (do ((i 1 (+ i 1))
       (h (u8vector-ref v 0) (+ h (u8vector-ref v i))))
      ((= i (u8vector-length v)) (hash h))))

(define (u8vec-eq? a b)
  (let ((len (u8vector-length a)))
    (if (= len (u8vector-length b))
        (let loop ((i 0))
          (if (= i len)
              #t
              (if (= (u8vector-ref a i) (u8vector-ref b i))
                  (loop (+ i 1))
                  #f)))
        #f)))

(define rules (make-hashtable hash-u8 u8vec-eq?))

(define (flip2x2 pattern)
  (u8vector (u8vector-ref pattern 2) (u8vector-ref pattern 3)
            (u8vector-ref pattern 0) (u8vector-ref pattern 1)))

(define (rotate2x2 pattern)
  (u8vector (u8vector-ref pattern 2) (u8vector-ref pattern 0)
            (u8vector-ref pattern 3) (u8vector-ref pattern 1)))

(define (flip3x3 pattern)
  (u8vector (u8vector-ref pattern 6) (u8vector-ref pattern 7) (u8vector-ref pattern 8)
            (u8vector-ref pattern 3) (u8vector-ref pattern 4) (u8vector-ref pattern 5)
            (u8vector-ref pattern 0) (u8vector-ref pattern 1) (u8vector-ref pattern 2)))

(define (rotate3x3 pattern)
  (u8vector (u8vector-ref pattern 6) (u8vector-ref pattern 3) (u8vector-ref pattern 0)
            (u8vector-ref pattern 7) (u8vector-ref pattern 4) (u8vector-ref pattern 1)
            (u8vector-ref pattern 8) (u8vector-ref pattern 5) (u8vector-ref pattern 2)))

(define (flip pattern)
  (if (= (remainder (u8vector-length pattern) 2) 0)
      (flip2x2 pattern)
      (flip3x3 pattern)))

(define (rotate pattern)
  (if (= (remainder (u8vector-length pattern) 2) 0)
      (rotate2x2 pattern)
      (rotate3x3 pattern)))

(define (char->light ch) (if (char=? ch #\#) 1 0))
(define re-2x2 (regex "^(../..) => ([.#/]+)$"))
(define re-3x3 (regex "^(.../.../...) => ([.#/]+)$"))

(define (filter-slash str) (string-remove (cut char=? #\/ <>) str))

(define (parse-pattern pat len)
  (let ((patvec (make-u8vector len)))
    (do ((i 0 (+ i 1)))
        ((= i len) patvec)
      (u8vector-set! patvec i (char->light (string-ref pat i))))))

(define (parse2x2 line)
  (let ((res (regex-match re-2x2 line)))
    (if res
        (let* ((pattern (filter-slash (second res)))
              (result (parse-pattern (filter-slash (third res)) 9))
              (patvec (parse-pattern pattern 4)))
          (hashtable-set! rules patvec result)
          #t)
        #f)))

(define (parse3x3 line)
  (let ((res (regex-match re-3x3 line)))
    (if res
        (let* ((pattern (filter-slash (second res)))
              (result (parse-pattern (filter-slash (third res)) 16))
              (patvec (parse-pattern pattern 9)))
          (hashtable-set! rules patvec result)
          #t)
        #f)))

(define (process-rule line)
  (if (parse2x2 line)
      #t
      (parse3x3 line)))

(define grid (vector (u8vector 0 1 0) (u8vector 0 0 1) (u8vector 1 1 1)))


(define (grid-ref grid x y) (u8vector-ref (vector-ref grid x) y))


(define (get2x2 grid row col)
  (let ((x (* row 2))
        (y (* col 2)))
    (u8vector (grid-ref grid x y) (grid-ref grid x (+ y 1))
              (grid-ref grid (+ x 1) y) (grid-ref grid (+ x 1) (+ y 1)))))

(define (set2x2-3x3! grid row col new)
  (let* ((x (* row 3))
         (y (* col 3))
         (row1 (vector-ref grid x))
         (row2 (vector-ref grid (+ x 1)))
         (row3 (vector-ref grid (+ x 2))))
    (u8vector-copy! row1 y new 0 3)
    (u8vector-copy! row2 y new 3 3)
    (u8vector-copy! row3 y new 6 3)))


(define (get3x3 grid row col)
  (let ((x (* row 3))
        (y (* col 3)))
    (u8vector (grid-ref grid x y) (grid-ref grid x (+ y 1)) (grid-ref grid x (+ y 2))
              (grid-ref grid (+ x 1) y) (grid-ref grid (+ x 1) (+ y 1)) (grid-ref grid (+ x 1) (+ y 2))
              (grid-ref grid (+ x 2) y) (grid-ref grid (+ x 2) (+ y 1)) (grid-ref grid (+ x 2) (+ y 2)))))


(define (set3x3-4x4! grid row col new)
  (let* ((x (* row 4))
         (y (* col 4))
         (row1 (vector-ref grid x))
         (row2 (vector-ref grid (+ x 1)))
         (row3 (vector-ref grid (+ x 2)))
         (row4 (vector-ref grid (+ x 3))))
    (u8vector-copy! row1 y new 0 4)
    (u8vector-copy! row2 y new 4 4)
    (u8vector-copy! row3 y new 8 4)
    (u8vector-copy! row4 y new 12 4)))

(define (make-grid x y)
  (let ((grid (make-vector x)))
    (do ((i 0 (+ i 1)))
        ((= i x) grid)
      (vector-set! grid i (make-u8vector y)))))

(define (lookup-rotations square)
  (let loop ((square square) (n 0))
    (if (= n 4)
        #f
        (let ((res (hashtable-ref rules square #f)))
          (if res
              res
              (loop (rotate square) (+ n 1)))))))

(define (lookup-rule square)
  (let ((res (lookup-rotations square)))
    (if res
        res
        (lookup-rotations (flip square)))))

(define (paint2x2 grid)
  (let* ((n (quotient (vector-length grid) 2))
         (newgrid (make-grid (* n 3) (* n 3))))
    (do ((row 0 (+ row 1)))
        ((= row n) newgrid)
      (do ((col 0 (+ col 1)))
          ((= col n))
        (let* ((square (get2x2 grid row col))
               (new-paint (lookup-rule square)))
          (if new-paint
              (set2x2-3x3! newgrid row col new-paint)
              (begin
                (format #t "Unable to find matching pattern for ~A~%" square)
                (exit 1))))))))

(define (paint3x3 grid)
  (let* ((n (quotient (vector-length grid) 3))
         (newgrid (make-grid (* n 4) (* n 4))))
    (do ((row 0 (+ row 1)))
        ((= row n) newgrid)
      (do ((col 0 (+ col 1)))
          ((= col n))
        (let* ((square (get3x3 grid row col))
               (new-paint (lookup-rule square)))
          (if new-paint
              (set3x3-4x4! newgrid row col new-paint)
              (begin
                (format #t "Unable to find matching pattern for ~A~%" square)
                (exit 1))))))))

(define (paint grid)
  (if (= (remainder (vector-length grid) 2) 0)
      (paint2x2 grid)
      (paint3x3 grid)))

(define (display-grid grid)
  (let ((len (vector-length grid)))
    (do ((x 0 (+ x 1)))
        ((= x len))
      (do ((y 0 (+ y 1)))
          ((= y len) (newline))
        (if (= (grid-ref grid x y) 1)
            (write-char #\#)
            (write-char #\.))))))

(define (count-lights grid)
  (let ((len (vector-length grid))
        (count 0))
    (do ((x 0 (+ x 1)))
        ((= x len) count)
      (do ((y 0 (+ y 1)))
          ((= y len))
        (if (= (grid-ref grid x y) 1) (set! count (+ count 1)))))))

(define (paint-steps grid n)
  (let loop ((i 1)
             (grid (paint grid)))
;;    (format #t "~%After ~A steps, ~Ax~A:~%" i (vector-length grid) (vector-length grid))
;;    (display-grid grid)
    (if (= i n)
        grid
        (loop (+ i 1) (paint grid)))))

(define starting-layout (vector
                         (u8vector 0 1 0)
                         (u8vector 0 0 1)
                         (u8vector 1 1 1)))

(for-each process-rule '("../.# => ##./#../..."
                         ".#./..#/### => #..#/..../..../#..#"))

(format #t "Test 1: ~A~%" (paint-steps starting-layout 2))

(hashtable-clear! rules)
(for-each process-rule (read-lines))
(define after5 (count-lights (paint-steps starting-layout 5)))
(format #t "Part 1: ~A~%" (count-lights after5))
(define after18 (paint-steps after5 13))
(format #t "Part 2: ~A~%" (count-lights after18))

