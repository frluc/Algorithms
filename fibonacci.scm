;; fibonacci sequence, recursive

(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 2))
         (fibonacci (- n 1)))))
