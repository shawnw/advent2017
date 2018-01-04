(import (srfi 1) (rnrs hashtables) (uvector-utils) (kawa
regex) (io-utils) (srfi 133))

(define rules (make-hashtable u8vector-hash u8vector=?))

(define (flip2x2 pattern::u8vector) ::u8vector
  (u8vector (u8vector-ref pattern 2) (u8vector-ref pattern 3)
            (u8vector-ref pattern 0) (u8vector-ref pattern 1)))

(define (rotate2x2 pattern::u8vector) ::u8vector
  (u8vector (u8vector-ref pattern 2) (u8vector-ref pattern 0)
            (u8vector-ref pattern 3) (u8vector-ref pattern 1)))

(define (flip3x3 pattern::u8vector) ::u8vector
  (u8vector (u8vector-ref pattern 6) (u8vector-ref pattern 7) (u8vector-ref pattern 8)
            (u8vector-ref pattern 3) (u8vector-ref pattern 4) (u8vector-ref pattern 5)
            (u8vector-ref pattern 0) (u8vector-ref pattern 1) (u8vector-ref pattern 2)))

(define (rotate3x3 pattern::u8vector) ::u8vector
  (u8vector (u8vector-ref pattern 6) (u8vector-ref pattern 3) (u8vector-ref pattern 0)
            (u8vector-ref pattern 7) (u8vector-ref pattern 4) (u8vector-ref pattern 1)
            (u8vector-ref pattern 8) (u8vector-ref pattern 5) (u8vector-ref pattern 2)))

(define (flip pattern::u8vector) ::u8vector
  (if (= (remainder (u8vector-length pattern) 2) 0)
      (flip2x2 pattern)
      (flip3x3 pattern)))

(define (rotate pattern::u8vector) ::u8vector
  (if (= (remainder (u8vector-length pattern) 2) 0)
      (rotate2x2 pattern)
      (rotate3x3 pattern)))

(define (char->light ch::char) ::ubyte (if (char=? ch #\#) 1 0))
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
          (do ((i 0 (+ i 1))
               (pat patvec (rotate2x2 pat)))
              ((= i 4))
            (hashtable-set! rules pat result))
          (do ((i 0 (+ i 1))
               (pat (flip2x2 patvec) (rotate2x2 pat)))
              ((= i 4))
            (hashtable-set! rules pat result))
          #t)
        #f)))

(define (parse3x3 line)
  (let ((res (regex-match re-3x3 line)))
    (if res
        (let* ((pattern (filter-slash (second res)))
              (result (parse-pattern (filter-slash (third res)) 16))
              (patvec (parse-pattern pattern 9)))
          (do ((i 0 (+ i 1))
               (pat patvec (rotate3x3 pat)))
              ((= i 4))
            (hashtable-set! rules pat result))
          (do ((i 0 (+ i 1))
               (pat (flip3x3 patvec) (rotate3x3 pat)))
              ((= i 4))
            (hashtable-set! rules pat result))
          #t)
        #f)))

(define (process-rule line)
  (if (parse2x2 line)
      #t
      (parse3x3 line)))

(define (grid-ref grid::vector x::int y::int) ::ubyte
  (u8vector-ref (vector-ref grid x) y))

(define (get2x2 grid::vector row::int col::int) ::u8vector
  (let ((x ::int (* row 2))
        (y ::int (* col 2)))
    (u8vector (grid-ref grid x y) (grid-ref grid x (+ y 1))
              (grid-ref grid (+ x 1) y) (grid-ref grid (+ x 1) (+ y 1)))))

(define (set2x2-3x3! grid::vector row::int col::int new::u8vector)
  (let* ((x ::int (* row 3))
         (y ::int (* col 3))
         (row1 (vector-ref grid x))
         (row2 (vector-ref grid (+ x 1)))
         (row3 (vector-ref grid (+ x 2))))
    (u8vector-copy! row1 y new 0 3)
    (u8vector-copy! row2 y new 3 3)
    (u8vector-copy! row3 y new 6 3)))

(define (get3x3 grid::vector row::int col::int) ::u8vector
  (let ((x ::int (* row 3))
        (y ::int (* col 3)))
    (u8vector (grid-ref grid x y) (grid-ref grid x (+ y 1)) (grid-ref grid x (+ y 2))
              (grid-ref grid (+ x 1) y) (grid-ref grid (+ x 1) (+ y 1)) (grid-ref grid (+ x 1) (+ y 2))
              (grid-ref grid (+ x 2) y) (grid-ref grid (+ x 2) (+ y 1)) (grid-ref grid (+ x 2) (+ y 2)))))

(define (set3x3-4x4! grid::vector row::int col::int new::u8vector)
  (let* ((x::int (* row 4))
         (y::int (* col 4))
         (row1 (vector-ref grid x))
         (row2 (vector-ref grid (+ x 1)))
         (row3 (vector-ref grid (+ x 2)))
         (row4 (vector-ref grid (+ x 3))))
    (u8vector-copy! row1 y new 0 4)
    (u8vector-copy! row2 y new 4 4)
    (u8vector-copy! row3 y new 8 4)
    (u8vector-copy! row4 y new 12 4)))

(define (make-grid x::int y::int) ::vector
  (vector-unfold (lambda (i) (make-u8vector y)) x))

(define (lookup-rule square) (hashtable-ref rules square #f))

(define (paint2x2 grid::vector)
  (let* ((n ::int (quotient (vector-length grid) 2))
         (newgrid (make-grid (* n 3) (* n 3))))
    (do ((row ::int 0 (+ row 1)))
        ((= row n) newgrid)
      (do ((col ::int 0 (+ col 1)))
          ((= col n))
        (let* ((square (get2x2 grid row col))
               (new-paint (lookup-rule square)))
          (if new-paint
              (set2x2-3x3! newgrid row col new-paint)
              (begin
                (format #t "Unable to find matching pattern for ~A~%" square)
                (exit 1))))))))

(define (paint3x3 grid::vector)
  (let* ((n ::int (quotient (vector-length grid) 3))
         (newgrid (make-grid (* n 4) (* n 4))))
    (do ((row ::int 0 (+ row 1)))
        ((= row n) newgrid)
      (do ((col ::int 0 (+ col 1)))
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
  (vector-for-each
   (lambda (line)
     (u8vector-for-each
      (lambda (p)
        (if (= p 1)
            (write-char #\#)
            (write-char #\.))) line)
     (newline))
   grid))

(define (count-lights grid)
  (vector-fold
   (lambda (sum line) (+ sum (u8vector-fold + 0 line))) 0 grid))

(define (paint-steps grid n)
  (let loop ((i 1)
             (grid (paint grid)))
;;    (format #t "~%After ~A steps, ~Ax~A:~%" i (vector-length grid) (vector-length grid))
;;    (display-grid grid)
    (if (= i n)
        grid
        (loop (+ i 1) (paint grid)))))

(define starting-layout (vector
                         '#u8(0 1 0)
                         '#u8(0 0 1)
                         '#u8(1 1 1)))

(for-each process-rule '("../.# => ##./#../..."
                         ".#./..#/### => #..#/..../..../#..#"))
(format #t "Test 1: ~A~%" (count-lights (paint-steps starting-layout 2)))

(hashtable-clear! rules)
(for-each process-rule (read-lines))
(define after5 (paint-steps starting-layout 5))
(format #t "Part 1: ~A~%" (count-lights after5))
(define after18 (paint-steps after5 13))
(format #t "Part 2: ~A~%" (count-lights after18))

