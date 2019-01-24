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


;; Tests below

;;(require racket/trace)
;;(trace first-n)

(first-n (seq 1 10) 0) ;; () (doesn't even need to be handled but it turns out I do
(first-n (seq 1 10) 5) ;; (1 2 3 4 5)
(first-n (seq 1 10) 11) ;; (1 2 3 4 5 6 7 8 9 10)
