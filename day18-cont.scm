(require-extension srfi-1)
(require-extension srfi-69)
(require-extension irregex)
(require-extension format)

(define (make-coroutine proc)
  (let* ((return #f)
         (resume #f)
         (yield (lambda (v) (call/cc (lambda (r) (set! resume r) (return v))))))
    (lambda ()
      (call/cc
       (lambda (cc)
         (set! return cc)
         (if resume
             (resume 'restarting)
             (begin
               (proc yield)
               (return #f))))))))


(define (get-value registers x)
  (if (char? x)
      (hash-table-ref/default registers x 0)
      x))

(define (parse-arg a)
  (if (and (= (string-length a) 1) (char-lower-case? (string-ref a 0)))
      (string-ref a 0)
      (string->number a)))

(define last-frequency 0)
(define prog1-send-count 0)

(define (snd x registers writer progn)
  (let ((freq (get-value registers x)))
    (if (queue? writer)
        (begin
          (when (= progn 1)
                (set! prog1-send-count (+ prog1-send-count 1)))
          (queue-add! writer freq))
        (set! last-frequency freq))
    1))
(define (set registers x y)
  (hash-table-set! registers x (get-value registers y))
  1)
(define (add registers x y)
  (hash-table-set! registers x (+ (get-value registers x) (get-value registers y)))
  1)
(define (mul registers x y)
  (hash-table-set! registers x (* (get-value registers x) (get-value registers y)))
  1)
(define (instrmod registers x y)
  (hash-table-set! registers x (remainder (get-value registers x) (get-value registers y)))
  1)
(define (rcv x registers reader yield)
  (let ((v (get-value registers x)))
    (if (queue? reader)
          (begin
            (when (queue-empty? reader)
                  (yield 'waiting))
            (hash-table-set! registers x (queue-remove! reader))
            1)
          (begin
            (when (not (= v 0))
                  (abort last-frequency))
            1))))
(define (jgz registers x y)
  (let ((valx (get-value registers x))
        (valy (get-value registers y)))
    (if (> valx 0)
        valy
        1)))

(define (compile-input cmds)
  (let ((snd-re (string->irregex "^snd ([a-z])$"))
        (set-re (string->irregex "^set ([a-z]) (-?\\d+|[a-z])$"))
        (add-re (string->irregex "^add ([a-z]) (-?\\d+|[a-z])$"))
        (mul-re (string->irregex "^mul ([a-z]) (-?\\d+|[a-z])$"))
        (mod-re (string->irregex "^mod ([a-z]) (-?\\d+|[a-z])$"))
        (rcv-re (string->irregex "^rcv ([a-z])$"))
        (jgz-re (string->irregex "^jgz (-?\\d+|[a-z]) (-?\\d+|[a-z])$")))
    (list->vector
     (map (lambda (instr)
            (cond
             ((irregex-match snd-re instr) =>
              (lambda (bits)
                (let ((x (parse-arg (irregex-match-substring bits 1))))
                  (lambda (r q1 q2 n yi) (snd x r q2 n)))))
             ((irregex-match set-re instr) =>
              (lambda (bits)
                (let ((x (parse-arg (irregex-match-substring bits 1)))
                      (y (parse-arg (irregex-match-substring bits 2))))
                  (lambda (r q1 q2 n yi) (set r x y)))))
             ((irregex-match add-re instr) =>
              (lambda (bits)
                (let ((x (parse-arg (irregex-match-substring bits 1)))
                      (y (parse-arg (irregex-match-substring bits 2))))
                  (lambda (r q1 q2 n yi) (add r x y)))))
             ((irregex-match mul-re instr) =>
              (lambda (bits)
                (let ((x (parse-arg (irregex-match-substring bits 1)))
                      (y (parse-arg (irregex-match-substring bits 2))))
                  (lambda (r q1 q2 n yi) (mul r x y)))))
             ((irregex-match mod-re instr) =>
              (lambda (bits)
                (let ((x (parse-arg (irregex-match-substring bits 1)))
                      (y (parse-arg (irregex-match-substring bits 2))))
                  (lambda (r q1 q2 n yi) (instrmod r x y)))))
             ((irregex-match rcv-re instr) =>
              (lambda (bits)
                (let ((x (parse-arg (irregex-match-substring bits 1))))
                  (lambda (r q1 q2 n yi) (rcv x r q1 yi)))))
             ((irregex-match jgz-re instr) =>
              (lambda (bits)
                (let ((x (parse-arg (irregex-match-substring bits 1)))
                      (y (parse-arg (irregex-match-substring bits 2))))
                  (lambda (r q1 q2 n yi) (jgz r x y)))))
             (else
              (error "Unknown instruction" instr))))
          cmds))))

(define program (compile-input (read-lines)))
(define opcount (vector-length program))

(call/cc
 (lambda (quit)
   (with-exception-handler
    (lambda (val)
      (format #t "Part 1: ~A~%" val)
      (quit '()))
    (lambda ()
      (let ((registers (make-hash-table test: char=? hash: (lambda (c b) (char->integer c)))))
        (let loop ((pc 0))
          (if (or (< pc 0) (>= pc opcount))
              (display "Program terminated.\n")
              (loop (+ pc ((vector-ref program pc) registers #f #f 0 #f))))))))))

(define (run program registers reader writer progn)
  (make-coroutine
   (lambda (yield)
     (let loop ((pc 0))
       (if (or (< pc 0) (>= pc opcount))
           (begin
             (format #t "Program ~A pc ~A~%" progn pc)
             #f)
           (let ((newpc ((vector-ref program pc) registers reader writer progn yield)))
                 (loop (+ pc newpc))))))))

(define (run-both)
  (let* ((registers1 (make-hash-table test: char=? hash: (lambda (c b) (char->integer c))))
        (queue1 (make-queue))
        (registers2 (make-hash-table test: char=? hash: (lambda (c b) (char->integer c))))
        (queue2 (make-queue))
        (gen1 (run program registers1 queue1 queue2 0))
        (gen2 (run program registers2 queue2 queue1 1)))
    (hash-table-set! registers1 #\p 0)
    (hash-table-set! registers2 #\p 1)
      (let loop ((ret1 (gen1))
                 (ret2 (gen2)))
        (cond
         ((boolean? ret1)
          (display "Program 0 terminated.\n"))
         ((boolean? ret2)
          (display "Program 1 terminated.\n"))
         ((and (queue-empty? queue1) (queue-empty? queue2)
               (eq? ret1 'waiting) (eq? ret2 'waiting))
          (display "Programs deadlocked.\n"))
         ((queue-empty? queue1)
          (loop ret1 (gen2)))
         ((queue-empty? queue2)
          (loop (gen1) ret2))
         (else
          (display "This shouldn't happen...\n"))))))

(run-both)
(format #t "Part 2: ~A~%" prog1-send-count)

