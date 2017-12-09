(import (kawa regex) (rnrs hashtables) (srfi 1))

(define instr-re (regex "^(\\w+) (inc|dec) (-?\\d+) if (\\w+) ([=<>!]+) (-?\\d+)\\s*$"))

(define (process-instruction symtab)
  (let ((line (read-line)))
    (cond
     ((eof-object? line) line)
     ((regex-match instr-re line) =>
      (lambda (fields)
        (let ((dest (second fields))
              (dir (string->symbol (third fields)))
              (amount (string->number (fourth fields)))
              (lop (hashtable-ref symtab (fifth fields) 0))
              (op (string->symbol (sixth fields)))
              (rop (string->number (seventh fields))))
          (if (case op
                ((>) (> lop rop))
                ((<) (< lop rop))
                ((>=) (>= lop rop))
                ((<=) (<= lop rop))
                ((==) (= lop rop))
                ((!=) (not (= lop rop)))
                (else
                 (error "Invalid instruction line" line op)))
              (let* ((destval (hashtable-ref symtab dest 0))
                     (newval (if (eq? dir 'inc) (+ destval amount) (- destval amount))))
                (hashtable-set! symtab dest newval)
                newval)
              0))))
     (else
      (error "Invalid instruction" line)))))

(define (find-largest table)
  (let-values (((keys entries) (hashtable-entries table)))
    (reduce max 0 (vector->list entries))))

(define (process-instructions)
  (let ((symtab (make-hashtable string-hash string=?)))
    (let loop ((res (process-instruction symtab))
               (maxval 0))
      (if (eof-object? res)
          (values (find-largest symtab) maxval)
          (loop (process-instruction symtab) (max maxval res))))))

(format #t "Part 1 and 2: ~A~%" (process-instructions))


