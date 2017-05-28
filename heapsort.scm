;; heap data structure & heapsort algorithm

(define (create-heap maxsize)
  (vector 'heap 0 (make-vector (+ 1 maxsize)))) ;; maxsize + 1 since we use vector indexes starting at 1

(define (heap-size heap)
  (vector-ref heap 1))

(define (heap-vector heap)
  (vector-ref heap 2))

(define (left-child-index i)
  (* 2 i))

(define (right-child-index i)
  (+ 1 (* 2 i)))

(define (parent-index i)
  (quotient i 2))

(define (swap vector i j)
  (let ((temp (vector-ref vector i)))
    (vector-set! vector i (vector-ref vector j))
    (vector-set! vector j temp)))

(define (heap-add! heap value) ;; add a value to a heap as a child node and correct heap conditions
  (vector-set! heap 1 (+ 1 (heap-size heap)))
  (vector-set! (heap-vector heap) (heap-size heap) value)
  (do ((i (heap-size heap) (parent-index i)))
      ((or (= i 1)
           (< (vector-ref (heap-vector heap) i)
              (vector-ref (heap-vector heap) (parent-index i))))
       heap)
    (swap (heap-vector heap) i (parent-index i))))

(define (heap-insert! heap value) ;; add a value as root node and correct heap conditions
  (if (< value (vector-ref (heap-vector heap) 1))
      (begin
        (vector-set! (heap-vector heap) 1 value)
        (let loop ((i 1) (l 2) (r 3))
          (if (or (and (<= l (heap-size heap))
                       (< (vector-ref (heap-vector heap) i)
                          (vector-ref (heap-vector heap) l)))
                  (and (<= r (heap-size heap))
                       (< (vector-ref (heap-vector heap) i)
                          (vector-ref (heap-vector heap) r))))
              (let ((m (if (> r (heap-size heap))
                           l
                           (if (< (vector-ref (heap-vector heap) l)
                                  (vector-ref (heap-vector heap) r))
                               r
                               l))))
                (swap (heap-vector heap) i m)
                (loop m (left-child-index m) (right-child-index m))))))))

(define (heapsort input-vector)
  (let ((heap (create-heap (vector-length input-vector)))
        (output-vector (make-vector (vector-length input-vector))))
    (do ((i 0 (+ i 1)))
        ((= i (vector-length input-vector)))
      (heap-add! heap (vector-ref input-vector i)))
    (do ((j 0 (+ j 1)))
        ((= j (vector-length input-vector)) output-vector)
      (begin
        (vector-set! output-vector j (vector-ref (heap-vector heap) 1))
        (heap-insert! heap (vector-ref (heap-vector heap) (heap-size heap)))
        (vector-set! heap 1 (- (heap-size heap) 1))))))
