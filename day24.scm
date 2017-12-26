(import (srfi 1) (io-utils) (kawa regex))

(define port-re (regex "^(\\d+)/(\\d+)$"))

(define (port->pair str)
  (let ((bits (regex-match port-re str)))
    (cons (string->number (second bits)) (string->number (third bits)))))

(define (matching-components n components)
  (filter (lambda (p) (or (= (car p) n) (= (cdr p) n))) components))

(define (pair-match a b)
  (or (and (= (car a) (car b)) (= (cdr a) (cdr b)))
      (and (= (car a) (cdr b)) (= (cdr a) (car b)))))

(define (strength bridge)
  (fold (lambda (port sum) (+ sum (car port) (cdr port))) 0 bridge))

(define (max-strength bridges)
  (fold (lambda (b m) (max m (strength b))) 0 bridges))

(define (max-length bridges)
  (fold (lambda (b m) (max m (length b))) 0 bridges))

(define (build-bridges start components)
  (let ((matching (matching-components start components)))
    (apply append (map (lambda (component)
                         (cons (list component)
                               (map (lambda (bridge) (cons component bridge))
                                    (build-bridges (if (= (car component) start)
                                                       (cdr component)
                                                       (car component))
                                                   (remove (cut eqv? component <>)
                                                           components)))))
                       matching))))

(define (longest-strength bridges)
  (let ((maxlen (max-length bridges)))
    (max-strength (filter (lambda (b) (= maxlen (length b))) bridges))))

(define test-input '((0 . 2) (2 . 2) (2 . 3) (3 . 4) (3 . 5) (0 . 1) (10 . 1) (9 . 10)))
(define test-bridges (build-bridges 0 test-input))
(format #t "Test 1: ~A~%" (max-strength test-bridges))

(define input (map port->pair (read-lines)))
(define bridges (build-bridges 0 input))
(format #t "Part 1: ~A~%" (max-strength bridges))

(format #t "Test 2: ~A~%" (longest-strength test-bridges))
(format #t "Part 2: ~A~%" (longest-strength bridges))
