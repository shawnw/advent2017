(import (kawa regex) (srfi 1))

(define layer-re (regex "^(\\d+):\\s+(\\d+)$"))
(define (read-layer str)
  (let ((parts (regex-match layer-re str)))
    (vector (string->number (second parts)) 0 (string->number (third parts)) 'forward)))

(define (read-layers)
  (let ((layers '()))
    (let loop ((line (read-line)))
      (if (eof-object? line)
          (reverse! layers)
          (begin
            (set! layers (cons (read-layer line) layers))
            (loop (read-line)))))))

(define (advance-scanners! firewall)
  (for-each (lambda (layer)
              (let* ((pos (vector-ref layer 1))
                    (depth (vector-ref layer 2))
                    (direction (vector-ref layer 3)))
                (cond
                ((and (= (+ pos 1) depth) (eq? direction 'forward))
                 (vector-set! layer 1 (- pos 1))
                 (vector-set! layer 3 'backwards))
                ((eq? direction 'forward)
                 (vector-set! layer 1 (+ pos 1)))
                ((and (= pos 0) (eq? direction 'backwards))
                 (vector-set! layer 1 1)
                 (vector-set! layer 3 'forward))
                ((eq? direction 'backwards)
                 (vector-set! layer 1 (- pos 1)))
                (else
                 (error "invalid state" (vector-ref layer 0))))))
            firewall))

(define (reset-scanners! firewall)
  (for-each (lambda (layer)
              (vector-set! layer 1 0)
              (vector-set! layer 3 'forward))
            firewall))

(define (firewall-copy firewall)
  (map vector-copy firewall))

(define (advance current layers score early-fail)
  (if (or (null? layers) (and early-fail (> score 0)))
      score
       (let ((layer (car layers)))
         (if (= current (vector-ref layer 0))
             (begin
               (when (= (vector-ref layer 1) 0)
                     (set! score (+ score (* current (vector-ref layer 2)))))
               (advance-scanners! (cdr layers))
               (advance (+ current 1) (cdr layers) score early-fail))
             (begin
               (advance-scanners! layers)
               (advance (+ current 1) layers score early-fail))))))


(define firewall (read-layers))
(format #t "Part 1: ~A~%" (advance 0 firewall 0 #f))
(reset-scanners! firewall)
(let loop ((n 1) (firewall firewall))
  (advance-scanners! firewall)
  (let ((curr-firewall (firewall-copy firewall)))
    (if (and (> (vector-ref (car firewall) 1) 0) (= (advance 0 firewall 0 #t) 0))
        (format #t "Part 2: ~A~%" n)
        (loop (+ n 1) curr-firewall))))

