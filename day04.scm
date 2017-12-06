(import (kawa regex) (rnrs hashtables) (srfi 1) (srfi 8) (srfi 132))

#;(define (check-passphrase1 words)
  (let ((table (make-hashtable string-hash string=?)))
    (let loop ((words words))
      (cond
       ((null? words)
        1)
       ((hashtable-contains? table (car words))
        0)
       (else
        (hashtable-set! table (car words) #t)
        (loop (cdr words)))))))

#;(define (check-passphrase2 words)
  (let ((table (make-hashtable string-hash string=?)))
    (let loop ((words words))
      (if (null? words)
          1
          (let ((ana (list->string (list-sort char<? (string->list (car words))))))
            (if
             (hashtable-contains? table ana)
             0
             (begin
               (hashtable-set! table ana #t)
               (loop (cdr words)))))))))

(define (check-passphrase1 words)
  (cond
   ((null? words) 1)
   ((member (car words) (cdr words) string=?) 0)
   (else
    (check-passphrase1 (cdr words)))))

(define (check-passphrase2 words)
  (let ((words (map (lambda (word)
                      (list->string (list-sort char<? (string->list word))))
                    words)))
    (check-passphrase1 words)))

(define (check-passphrases)
  (let loop ((line (read-line))
             (total1 0)
             (total2 0))
    (if (eof-object? line)
        (values total1 total2)
        (let ((words (regex-split "\\s+" line)))
          (loop (read-line)
                (+ total1 (check-passphrase1 words))
                (+ total2 (check-passphrase2 words)))))))
(receive (part1 part2) (check-passphrases)
         (format #t "Part 1: ~A~%Part 2: ~A~%" part1 part2))
