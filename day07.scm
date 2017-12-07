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
                       (sedges (cadddr internal))
                       (entry (assq name graph))
                       (edges (map string->symbol (regex-split ",\\s+" sedges))))
                  (if entry
                      (begin
                        (vector-set! (cdr entry) 0 weight)
                        (vector-set! (cdr entry) 1 edges))
                      (set! graph (alist-cons name (vector weight edges #f) graph))))))
             (else
              (error "Bad line" line)))
            (loop (read-line)))))))

(define (weight graph root)
  (let* ((entry (assq root graph))
         (rec (cdr entry))
         (cached-weight (vector-ref rec 2)))
    (if cached-weight
        cached-weight
        (let ((calc-weight
               (fold (lambda (node acc)
                       (+ acc (weight graph node)))
                     (vector-ref rec 0)
                     (vector-ref rec 1))))
          (vector-set! rec 2 calc-weight)
          calc-weight))))

(define (create-dot graph)
  (display "digraph robots {\n")
  (let ((unbalanced '()))
    (for-each
     (lambda (node)
       (let* ((rec (cdr node))
              (links (vector-ref rec 1))
              (my-weight (weight graph (car node)))
              (child-weights (map (lambda (child) (weight graph child)) links))
              (color
               (if (> (length child-weights) 1)
                   (let ((this-weight (car child-weights)))
                     (if (every (lambda (that-weight)
                                  (= this-weight that-weight))
                                child-weights)
                         'black
                         'red))
                   'black)))
         (format #t "~A [label=\"name: ~A\\nweight: ~A\\ntree weight: ~A\""
                 (car node) (car node) (vector-ref (cdr node) 0) (vector-ref (cdr node) 2))
         (if (or (eq? 'red color) (memq (car node) unbalanced))
             (display "style=filled; fillcolor=red"))
         (display "]\n")
         (for-each (lambda (link weight)
                     (format #t "~A -> ~A" (car node) link)
                     (when (and (> (length child-weights) 1)
                                (= 1 (count (lambda (edge) (= edge weight)) child-weights)))
                           (set! unbalanced (cons link unbalanced))
                           (display " [color=red]"))
                     (newline))
                   links child-weights)))
     graph))
  (display "}\n"))

(create-dot (read-graph))
