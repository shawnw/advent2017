(import (srfi 1) (srfi 126) (kawa regex))

(define-record-type vertex (make-vertex name edges)
  vertex?
  (name get-name)
  (edges get-edges set-edges!))

(define line-re (regex "^(\\d+)\\s+<->\\s+([0-9, ]+)\\s*$"))
(define (parse-line line)
  (let ((bits (regex-match line-re line)))
    (if bits
        (cons (string->number (second bits)) (map string->number (string-split (third bits) ", ")))
        (error "Failed to match line" line))))

(define (add-edge! graph from to)
  (let ((set-one-edge!
         (lambda (graph from to)
           (let ((v (hashtable-ref graph from #f)))
             (if v
                 (set-edges! v (cons to (get-edges v)))
                 (hashtable-set! graph from (make-vertex from (list to))))))))
    (set-one-edge! graph from to)
    (set-one-edge! graph to from)))

(define (read-graph)
  (let ((graph (make-hashtable (lambda (x) x) =))
        (line (read-line)))
    (let loop ((line line))
      (if (eof-object? line)
          graph
          (let* ((components (parse-line line))
                 (origin (car components)))
            (for-each (lambda (edge) (add-edge! graph origin edge)) (cdr components))
            (loop (read-line)))))))


(define (extract-cycle graph root #!optional (connected (make-hashtable (lambda (x) x) =)))
  (let ((rootv (hashtable-ref graph root #f)))
    (when rootv
          (hashtable-set! connected (get-name rootv) #t)
          (for-each (lambda (dest)
                      (let ((dest-node (hashtable-ref graph dest #f)))
                        (unless (hashtable-contains? connected dest)
                                (extract-cycle graph dest connected))))
                    (get-edges rootv)))
          connected))

(define (remove-cycle! graph root)
  (let ((cycle (extract-cycle graph root)))
    (hashtable-prune! graph (lambda (k v) (hashtable-contains? cycle k)))))

(define (count-cycles graph)
  (let ((graph (hashtable-copy graph #t)))
    (do ((count 0 (+ count 1)))
        ((hashtable-empty? graph) count)
      (let-values (((root v found?) (hashtable-find graph (lambda (k v) #t))))
        (remove-cycle! graph root)))))

(define graph (read-graph))
(define cycle0 (extract-cycle graph 0))
(format #t "Part 1: ~A~%" (hashtable-size cycle0))
(format #t "Part 2: ~A~%" (count-cycles graph))


