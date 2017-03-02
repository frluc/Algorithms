;; linear maximum vector value search O(n)

(define (vector-max V)
  (let ((max (vector-ref V 0))
        (length (vector-length V)))
    (do ((i 1 (+ i 1)))
        ((= i length) (display max))
      (if (> (vector-ref V i) max)
          (set! max (vector-ref V i))))))
