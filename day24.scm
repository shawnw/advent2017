(import (srfi 1) (io-utils) (kawa regex))

(define port-re (regex "^(\\d+)/(\\d+)$"))

(define (port->pair str)
  (let ((bits (regex-match port-re str)))
    (cons (string->number (second bits)) (string->number (third bits)))))

(define (matching-components n components)
  (filter (lambda (p) (or (= (car p) n) (= (cdr p) n))) components))

(define (strength bridge)
  (fold (lambda (port sum) (+ sum (car port) (cdr port))) 0 bridge))

(define (max-strength bridges)
  (fold (lambda (b m) (max m (strength b))) 0 bridges))

(define (max-length bridges)
  (fold (lambda (b m) (max m (length b))) 0 bridges))

(define (remove-first pred? lst)
  (cond
   ((null? lst) '())
   ((pred? (car lst)) (cdr lst))
   (else
    (cons (car lst) (remove-first pred? (cdr lst))))))

(define (build-bridges-helper start components)
  (let ((matching (matching-components start components)))
    (append-map! (lambda (component)
                  (cons (list component)
                        (map! (lambda (bridge) (cons component bridge))
                             (build-bridges-helper (if (= (car component) start)
                                                (cdr component)
                                                (car component))
                                            (remove-first (cut eq? component <>)
                                                    components)))))
                matching)))

(define (build-bridges start components)
  (let*-values (((dups nodups) (partition (lambda (c) (= (car c) (cdr c))) components))
                ((bridges) (build-bridges-helper start nodups)))
    (map! (lambda (bridge)
            (fold (lambda (dup b)
                    (if (any (lambda (c) (or (= (car c) (car dup))
                                             (= (cdr c) (car dup)))) b)
                        (cons dup b)
                        b))
                  bridge dups))
          bridges)))

(define (longest-strength bridges)
  (let* ((schwartz (zip bridges (map length bridges)))
         (maxlen (fold (lambda (p maxsofar) (max (second p) maxsofar)) 0 schwartz))
         (maxbridges (unzip1 (filter (lambda (p) (= maxlen (second p))) schwartz))))
    (max-strength maxbridges)))

(define test-input '((0 . 2) (2 . 2) (2 . 3) (3 . 4) (3 . 5) (0 . 1) (10 . 1) (9 . 10)))
(define test-bridges (build-bridges 0 test-input))
(format #t "Test 1: ~A~%" (max-strength test-bridges))

(define input (map port->pair (read-lines)))
(define bridges (build-bridges 0 input))
(format #t "Part 1: ~A~%" (max-strength bridges))

(format #t "Test 2: ~A~%" (longest-strength test-bridges))
(format #t "Part 2: ~A~%" (longest-strength bridges))
