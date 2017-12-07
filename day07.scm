(import (kawa regex) (srfi 1))

(define (read-graph)
  (let ((graph '()))
    (let loop ((line (read-line)))
      (if (eof-object? line)
          graph
          (begin
            (cond
             ((regex-match "^(\\w+) \\((\\d+)\\)\\s*$" line) =>
              (lambda (leaf)
                (let* ((name (string->symbol (cadr leaf)))
                       (weight (string->number (caddr leaf)))
                       (entry (assq name graph)))
                  (if entry
                      (vector-set! (cdr entry) 0 weight)
                      (set! graph (alist-cons name (vector weight '() #f) graph))))))
             ((regex-match "^(\\w+)\\s+\\((\\d+)\\)\\s+->\\s+([a-z, ]+)\\s*$" line) =>
              (lambda (internal)
                (let* ((name (string->symbol (cadr internal)))
                       (weight (string->number (caddr internal)))
                       (sstanding (cadddr internal))
                       (entry (assq name graph))
                       (standing (map string->symbol (regex-split ",\\s+" sstanding))))
                  (if entry
                      (begin
                        (vector-set! (cdr entry) 0 weight)
                        (vector-set! (cdr entry) 1 standing))
                      (set! graph (alist-cons name (vector weight standing #f) graph))))))
             (else
              (error "Bad line" line)))
            (loop (read-line)))))))

(define (weight graph root)
  (let* ((entry (assq root graph))
         (cached-weight (vector-ref (cdr entry) 2)))
    (if cached-weight
        cached-weight
        (let ((calc-weight
               (fold (lambda (node acc)
                       (+ acc (weight graph node)))
                     (vector-ref (cdr entry) 0)
                     (vector-ref (cdr entry) 1))))
          (vector-set! (cdr entry) 2 calc-weight)
          calc-weight))))

(define graph (read-graph))

(display "digraph robots {\n")
(for-each
 (lambda (node)
   (let* ((rec (cdr node))
          (links (vector-ref rec 1))
          (my-weight (weight graph (car node)))
          (color
           (if (> (length links) 1)
               (let ((this-weight (weight graph (car links))))
                 (if (every (lambda (n)
                              (= this-weight (weight graph n)))
                            (cdr links))
                     'black
                     'red))
               'black)))
            (format #t "~A [label=\"name: ~A\\nweight: ~A\\ntree weight: ~A\""
                    (car node) (car node) (vector-ref (cdr node) 0) (vector-ref (cdr node) 2))
            (if (eq? 'red color)
                (display "style=filled; fillcolor=red"))
            (display "]\n")
            (for-each (lambda (link)
                        (format #t "~A -> ~A" (car node) link)
                        (if (eq? 'red color)
                            (display " [color=red]"))
                        (newline))
                      links)))
          graph)
(display "}\n")
