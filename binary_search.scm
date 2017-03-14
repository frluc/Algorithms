;; binary search, recursive

(define (binary-search vector value)
  (let boucle ((start 0)
               (end (- (vector-length vector) 1)))
    (if (> start end)
        -1
        (let ((pivot (quotient (+ end start) 2)))
          (cond ((> (vector-ref vector pivot) value)
                 (boucle start (- pivot 1)))
                ((< (vector-ref vector pivot) value)
                 (boucle (+ pivot 1) end))
                ((= (vector-ref vector pivot) value)
                 (print pivot)))))))           
