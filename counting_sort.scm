;; counting sort algorithm

(define (vector-min input-vector)
  (let ((min (vector-ref input-vector 0)))
    (do ((i 1 (+ i 1)))
        ((= i (vector-length input-vector)) min)
      (if (< (vector-ref input-vector i) min)
          (set! min (vector-ref input-vector i))))))

(define (vector-max input-vector)
  (let ((max (vector-ref input-vector 0)))
    (do ((i 1 (+ i 1)))
        ((= i (vector-length input-vector)) max)
      (if (> (vector-ref input-vector i) max)
          (set! max (vector-ref input-vector i))))))

(define (counting-sort input-vector)
  (let* ((Vmin (vector-min input-vector))
        (Vmax (vector-max input-vector))
        (output-vector (make-vector (vector-length input-vector)))
        (count-vector (make-vector (+ (- Vmax Vmin) 1) 0)))
    (do ((i 0 (+ i 1)))
        ((= i (vector-length input-vector)))
      (vector-set! count-vector
                   (- (vector-ref input-vector i) Vmin)
                   (+ (vector-ref count-vector (- (vector-ref input-vector i) Vmin)) 1)))
    (do ((j 1 (+ j 1)))
        ((= j (vector-length count-vector)))
      (vector-set! count-vector
                   j
                   (+ (vector-ref count-vector j) (vector-ref count-vector (- j 1)))))
    (do ((k (- (vector-length input-vector) 1) (- k 1)))
        ((< k 0) output-vector)
      (begin
        (vector-set! output-vector
                     (- (vector-ref count-vector (- (vector-ref input-vector k) Vmin)) 1)
                     (vector-ref input-vector k))
        (vector-set! count-vector
                     (- (vector-ref input-vector k) Vmin)
                     (- (vector-ref count-vector (- (vector-ref input-vector k) Vmin)) 1))))))
