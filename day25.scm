(import (kawa regex) (srfi 1) (collections))

(define start-state-re (regex "^Begin in state ([A-Z])\\.$"))
(define reps-re (regex "^Perform a diagnostic checksum after (\\d+) steps\\.$"))
(define in-state-re (regex "In state ([A-Z]):"))
(define state-current-re (regex "If the current value is ([01]):"))
(define write-re (regex "- Write the value ([01])\\."))
(define move-re (regex "- Move one slot to the (left|right)\\."))
(define transition-re (regex "- Continue with state ([A-Z])\\."))

(define (state->index ch) (- (char->integer ch) (char->integer #\A)))

(define (read-directions)
  (let* ((curr-state (string->number (second (regex-match state-current-re (read-line)))))
         (write-instr (string->number (second (regex-match write-re (read-line)))))
         (move-instr (string->symbol (second (regex-match move-re (read-line)))))
         (trans-instr (state->index (string-ref (second (regex-match transition-re (read-line))) 0))))
    (vector write-instr move-instr trans-instr)))

(define (read-states)
  (reverse!
   (let loop ((states '()))
     (let ((line (read-line)))
       (cond
        ((eof-object? line)
         states)
       ((= (string-length line) 0)
        (loop states))
       (else
        (let* ((state (string-ref (second (regex-match in-state-re line)) 0))
               (zero (read-directions))
               (one (read-directions)))
          (loop (cons (vector zero one) states)))))))))
          
(define (read-machine)
  (let* ((initial-state (string-ref (second (regex-match start-state-re (read-line))) 0))
         (reps (string->number (second (regex-match reps-re (read-line)))))
         (states (list->vector (read-states))))
    (values (state->index initial-state) states reps)))

(define (tape-move-left tape pos::int) ::int
  (if (= pos 0)
      (begin
        (coll-list-add-at! tape 0 0)
        0)
      (- pos 1)))

(define (tape-move-right tape pos::int) ::int
  (let ((newpos (+ pos 1)))
    (if (= newpos (coll-size tape))
        (begin
          (coll-add! tape 0)
          newpos)
        newpos)))

(define (tape-checksum tape) (coll-count tape 1))

(define (run-machine states starting reps::int)
  (let* ((tape (coll-make-array-list (coll-immut-list 10000 0))))
    (let loop ((n ::int 0) (state ::int starting) (pos ::int 6200))
      (if (= n reps)
          (values (tape-checksum tape) (coll-size tape))
          (let ((dirs (vector-ref (vector-ref states state) (coll-list-get tape pos))))
            (coll-list-set! tape pos (vector-ref dirs 0))
            (loop (+ n 1) (vector-ref dirs 2) (if (eq? (vector-ref dirs 1) 'left)
                                                  (tape-move-left tape pos)
                                                  (tape-move-right tape pos))))))))

(receive (start states reps) (read-machine)
         (format #t "Part 1: ~A~%" (run-machine states start reps)))

