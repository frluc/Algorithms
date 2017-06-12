;; basic matrix implementation using vectors

(define (matrix-tag) 'matrix)

(define (make-matrix row col)
  (let ((matrix (cons (matrix-tag) (make-vector row))))
    (do ((i 0 (+ 1 i)))
        ((= i row) matrix)
      (vector-set! (cdr matrix) i (make-vector col)))))

(define (matrix? object)
  (if (and
       (pair? object)
       (eq? (car object) 'matrix)
       (vector? (cdr object)))
      #t
      #f))

(define (matrix-ref matrix row col)
  (vector-ref (vector-ref (cdr matrix) row) col))

(define (matrix-set! matrix row col value)
  (vector-set! (vector-ref (cdr matrix) row) col value))
