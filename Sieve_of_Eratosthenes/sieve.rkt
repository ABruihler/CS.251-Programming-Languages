;; Function that generates a lazy list starting at first and ending at last.
(define seq
  (lambda (first last)
    (if (> first last)
        #f
        (cons first
            (lambda () (seq (+ first 1) last))))))

;; Function that returns a lazy list starting at first and never ending(define seq
(define inf-seq
  (lambda (first)
    (cons
     first
     (lambda () (inf-seq (+ first 1))))))

;; Returns the first n entries in the given lazy list.  If there are fewer entries in lazy-list than n, then all values in the lazy list are returned
(define first-n
  (lambda (lazy-list n)
    (cond
      ((null? lazy-list) ())
      ((zero? n) ())
      ((not lazy-list) ())
      (else (append (list (car lazy-list)) (first-n ((cdr lazy-list)) (- n 1)))))))

;; Returns the n-th value in the given lazy list.  If invalid n, then returns #f
(define nth
  (lambda (lazy-list n)
    (cond
      ((< n 1) #f)
      ((not lazy-list) #f)
      ((equal? n 1) (car lazy-list))
      (else (nth ((cdr lazy-list)) (- n 1))))))

;; TODO
(define filter-multiples
  (lambda (lazy-list n)
    (cond
      ((not lazy-list) ())
      ((zero? (modulo (car lazy-list) n)) (filter-multiples ((cdr lazy-list)) n))
      (else (cons
         (car lazy-list)
         (filter-multiples ((cdr lazy-list)) n))))))



;; Tests below

(require racket/trace)
;;(trace filter-multiples)

(first-n (seq 1 10) 0) ;; () (doesn't even need to be handled but it turns out I do)
(first-n (seq 1 10) 1) ;; (1)
(first-n (seq 1 10) 5) ;; (1 2 3 4 5)
(first-n (seq 1 10) 11) ;; (1 2 3 4 5 6 7 8 9 10)

(newline)
(nth (seq 1 10) 0) ;; #f
(nth (seq 1 10) 1) ;; 1
(nth (seq 1 10) 5) ;; 5
(nth (seq 1 10) 10) ;; 10
(nth (seq 1 10) 11) ;; #f

(newline)
(filter-multiples (seq 2 6) 2) ;; (3 5)
(filter-multiples (seq 3 8) 3) ;; (4 5 7 8)
(filter-multiples (seq 1 10) 11) ;; (1 2 3 4 5 6 7 8 9 10)
(filter-multiples (seq 1 10) 1) ;; ()
