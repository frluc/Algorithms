;; Stack data structure implementation

(define (create-stack)
  (vector 'stack '()))

(define (stack-list stack) ;return stacked elements list
  (vector-ref stack 1))

(define (stack? object) ;stack predicate
  (if (and
       (vector? object)
       (= (vector-length object) 2)
       (equal? (vector-ref object 0) 'stack)
       (list? (vector-ref object 1)))
      #t
      #f))

(define (empty-stack? stack)
  (if (null? (stack-list stack))
      #t
      #f))

(define (stack-list-set! stack list)
  (if (list? list)
      (vector-set! stack 1 list)
      (error "stack-list-set!" "not a list" list)))

(define (stack-push! stack value)
  (stack-list-set! stack (cons value (stack-list stack))))

(define (stack-top stack) ;peek
  (car (stack-list stack)))

(define (stack-bottom stack)
  (cdr (stack-list stack)))

(define (stack-pop! stack)
  (if (null? (stack-list stack))
      (error "stack-pop!" "empty stack" stack)
      (let ((value (stack-top stack)))
        (stack-list-set! stack (cdr (stack-list stack)))
        value)))

;; Reversing a vector storing values in a stack

(define (vector-reverse! vector)
  (let ((value-storage (create-stack)))
    (begin
      (do ((i 0 (+ i 1)))
          ((= i (vector-length vector)))
        (stack-push! value-storage (vector-ref vector i)))
      (do ((i 0 (+ i 1)))
          ((= i (vector-length vector)) vector)
        (vector-set! vector i (stack-pop! value-storage))))))    
