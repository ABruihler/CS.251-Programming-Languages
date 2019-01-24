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
