;; string matching "brute force" algorithm

(define (string-match-bf String Pattern)
  (let ((SL (string-length String))
        (PL (string-length Pattern)))
    (do ((i 0 (+ i 1)))
        ((= i (+ (- SL PL) 1)) (print "done"))
      (let ((word-test #t))
        (do ((j 0 (+ j 1)))
            ((= j PL)
             (if word-test
                 (print i)))
          (if (char=? (string-ref String (+ i j))
                      (string-ref Pattern j))
              (set! word-test (and word-test #t))
              (set! word-test (and word-test #f))))))))

(define (string-match-bf String Pattern)
  (let ((SL (string-length String))
        (PL (string-length Pattern))
        (result '()))
    (let loop ((i 0))
      (if (> i (- SL PL))
          (if (null? result)
              #f
              (reverse result))
          (begin
            (if (string=? Pattern (substring String i (+ i PL)))
            (begin
              (set! result (cons i result))
              (loop (+ i 1)))
            (loop (+ i 1))))))))
