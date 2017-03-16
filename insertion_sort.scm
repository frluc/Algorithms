;; insertion sort algorithm, iterative

(define (insertion_sort Vector)
  (do ((i 1 (+ i 1)))
      ((>= i (vector-length Vector)) Vector)
    (let ((j i))
      (let loop ()
        (if (and (> j 0)
                 (> (vector-ref Vector (- j 1))
                    (vector-ref Vector j)))
            (let ((temp (vector-ref Vector j)))
              (begin
                (vector-set! Vector j (vector-ref Vector (- j 1)))
                (vector-set! Vector (- j 1) temp)
                (set! j (- j 1))
                (loop))))))))


;; insertion sort algorithm, recursive

(define (insertion_sort_recursive List_to_sort)
  (define (insert value List)
    (if (null? List)
        (list value)
        (if (> value (car List))
            (cons value List)
            (cons (car List)
                  (insert value (cdr List))))))
  (if (null? (cdr List_to_sort))
      List_to_sort
      (insert (car List_to_sort)
              (insertion_sort_recursive (cdr List_to_sort)))))
