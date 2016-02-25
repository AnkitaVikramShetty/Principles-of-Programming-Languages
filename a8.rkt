#lang racket
(print-as-expression #f)
(require racket/trace)

(define empty-k
  (lambda ()
    `(empty-k)))
 
;;------------------------REGISTERIZATION OF 'ACK'------------------------------------------------------
#;(define ack
    (lambda (m n k)
      (cond
        [(zero? m) (k (add1 n))]
        [(zero? n) (ack (sub1 m) 1 k)]
        [else (ack m (sub1 n) (lambda (v) (ack (sub1 m) v k)))]))) 

(define ack-k* #f)
(define ack-m* #f)
(define ack-n* #f)
(define ack-v* #f)

(define ack
  (lambda ()
    (cond 
      [(zero? ack-m*) (begin
                        (set! ack-k* ack-k*)
                        (set! ack-v* (add1 ack-n*))
                        (ack-apply-k))]
      
      [(zero? ack-n*) (begin
                        (set! ack-k* ack-k*)
                        (set! ack-n* 1)
                        (set! ack-m* (sub1 ack-m*))
                        (ack))]
      [else (begin
              (set! ack-k* (ack-inner-k ack-m* ack-k*))
              (set! ack-m* ack-m*)
              (set! ack-n* (sub1 ack-n*))
              (ack))]))) 

(define ack-apply-k
  (lambda ()
    (match ack-k*
      [`(empty-k) ack-v*]
      [`(ack-inner-k ,m ,k) (begin
                              (set! ack-m* (sub1 m)) 
                              (set! ack-n* ack-v*)
                              (set! ack-k* k)
                              (ack))])))

(define ack-inner-k
  (lambda (m k)
    `(ack-inner-k ,m ,k)))

(define ack-reg-driver
  (lambda (m n)
    (begin
      (set! ack-m* m)
      (set! ack-n* n)
      (set! ack-k* (empty-k))
      (ack))))

;;------------------------REGISTERIZATION OF 'DEPTH'------------------------------------------------------

#;(define depth
    (lambda (ls k)
      (cond
        [(null? ls) (k 1)]
        [(pair? (car ls))
         (depth (car ls)
                (lambda (l)
                  (depth (cdr ls)
                         (lambda (r)
                           (let ((l (add1 l)))
                             (if (< l r) (k r) (k l)))))))]
        [else (depth (cdr ls) k)])))

(define depth-ls* #f)
(define depth-k* #f)
(define depth-l* #f)
(define depth-v* #f) 
 
(define depth
  (lambda ()
    (cond
      [(null? depth-ls*) (begin
                           (set! depth-k* depth-k*)
                           (set! depth-v* 1)
                           (depth-apply-k))] 
      [(pair? (car depth-ls*)) (begin
                                 (set! depth-k* (outer-depth-k depth-ls* depth-k*))
                                 (set! depth-ls* (car depth-ls*))
                                 (depth))]
      [else (begin
              (set! depth-k* depth-k*)
              (set! depth-ls* (cdr depth-ls*))
              (depth))])))

(define depth-apply-k
  (lambda ()
    (match depth-k*
      [`(empty-k) depth-v*]
      [`(inner-depth-k ,l ,k) (let ((l (add1 l)))
                                (begin
                                  (if (< l depth-v*)
                                      (begin
                                        (set! depth-k* k)
                                        (set! depth-v* depth-v*)
                                        (depth-apply-k))
                                      (begin
                                        (set! depth-k* k)
                                        (set! depth-v* l)
                                        (depth-apply-k)))))] 
      [`(outer-depth-k ,ls ,k) (begin
                                 (set! depth-ls* (cdr ls))
                                 (set! depth-k* (inner-depth-k depth-v* k)) 
                                 (depth))])))

(define inner-depth-k
  (lambda (l k)
    `(inner-depth-k ,l ,k)))

(define outer-depth-k
  (lambda (ls k)
    `(outer-depth-k ,ls ,k)))

(define depth-reg-driver
  (lambda (ls)
    (begin
      (set! depth-ls* ls)
      (set! depth-k* (empty-k))
      (depth))))

;;------------------------REGISTERIZATION OF 'FACT'------------------------------------------------------

#; (define fact
     (lambda (n k)
       ((lambda (fact k)
          (fact fact n k))
        (lambda (fact n k)
          (cond
            [(zero? n) (k 1)]
            [else (fact fact (sub1 n) (lambda (v) (k (* n v))))]))
        k)))

(define fact-n* #f)
(define fact-k* #f)
(define fact-v* #f)

(define fact
  (lambda ()
    ((lambda (fact) 
       (fact fact)) 
     (lambda (fact)
       (cond
         [(zero? fact-n*) (begin (set! fact-k* fact-k*) (set! fact-v* 1) (fact-apply-k))]
         [else (begin (set! fact-k* (inner-k-fact fact-n* fact-k*)) (set! fact-n* (sub1 fact-n*)) (fact fact))]))))) 


(define fact-apply-k
  (lambda ()
    (match fact-k*
      [`(empty-k) fact-v*]
      [`(inner-k-fact ,n ,k) (begin (set! fact-k* k) (set! fact-v* (* n fact-v*)) (fact-apply-k))]))) 

(define inner-k-fact
  (lambda (n k)
    `(inner-k-fact ,n ,k)))

(define fact-reg-driver
  (lambda (n)
    (begin
      (set! fact-n* n)
      (set! fact-k* (empty-k))
      (fact))))

;;------------------------REGISTERIZATION OF 'PASCAL'------------------------------------------------------

#;(define pascal
    (lambda (n k)
      (let ((pascal
             (lambda (pascal k)
               (k (lambda (m a k)
                    (cond
                      [(> m n) (k '())]
                      [else (let ((a (+ a m)))
                              (pascal pascal (lambda (f) (f (add1 m) a (lambda (v) (k (cons a v)))))))]))))))
        (pascal pascal (lambda (f) (f 1 0 k))))))

(define pascal-n* #f)
(define pascal-k* #f)
(define pascal-v* #f)


(define pascal
  (lambda ()
    (let ((pascal
           (lambda (pascal)
             (begin (set! pascal-v* (lambda (m a)
                                      (cond
                                        [(> m pascal-n*) (begin (set! pascal-v* '()) (set! pascal-k* pascal-k*) (pascal-apply-k))]
                                        [else (let ((a (+ a m)))
                                                (begin (set! pascal-k* (outer-k-pascal m a pascal-k*)) (pascal pascal)))]))) (pascal-apply-k))))) 
      (begin (set! pascal-k* (just-k-pascal pascal-k*)) (pascal pascal)))))

(define pascal-apply-k
  (lambda ()
    (match pascal-k*
      [`(empty-k) pascal-v*] 
      [`(inner-k-pascal ,a^ ,k^) (begin (set! pascal-k* k^) (set! pascal-v* (cons a^ pascal-v*)) (pascal-apply-k))]
      [`(just-k-pascal ,k^) (begin (set! pascal-k* k^) (pascal-v* 1 0))]
      [`(outer-k-pascal ,m^ ,a^ ,k^) (begin (set! pascal-k* (inner-k-pascal a^ k^)) (pascal-v* (add1 m^) a^))]
      )))

(define inner-k-pascal
  (lambda (a^ k^)
    `(inner-k-pascal ,a^ ,k^)))

(define just-k-pascal
  (lambda (k^)
    `(just-k-pascal ,k^)))

(define outer-k-pascal
  (lambda (m^ a^ k^)
    `(outer-k-pascal ,m^ ,a^ ,k^))) 

(define pascal-reg-driver
  (lambda (n)
    (begin
      (set! pascal-n* n)
      (set! pascal-k* (empty-k))
      (pascal))))

;;------------------------BRAINTEASER------------------------------------------------------


(define fib
  (lambda (n k)
    (cond
      [(= n 0) (lambda () (rampoline-apply-k k 1))]
      [(= n 1) (lambda () (rampoline-apply-k k 1))]
      [else (lambda () (fib (sub1 (sub1 n)) (outer-k-fib n k)))])))

(define rampoline-apply-k
  (lambda (k v)
    (match k
      [`(rampoline-empty-k ,jumpout) (lambda () (jumpout v))]
      [`(inner-k-fib ,v^ ,k^) (lambda () (rampoline-apply-k k^ (+ v v^)))]
      [`(outer-k-fib ,n^ ,k^) (lambda () (fib (sub1 n^) (inner-k-fib v k^)))])))

(define rampoline-empty-k
  (lambda (jumpout) `(rampoline-empty-k ,jumpout)))

(define inner-k-fib
  (lambda (v^ k^)
    `(inner-k-fib ,v^ ,k^)))

(define outer-k-fib
  (lambda (n^ k^)
    `(outer-k-fib ,n^ ,k^)))

(define fib-ramp-driver
  (lambda (n1 n2 n3)
    (call/cc
     (lambda (jumpout)
       (rampoline
        (lambda ()
          (fib n1 (rampoline-empty-k jumpout)))
        (lambda ()
          (fib n2 (rampoline-empty-k jumpout)))
        (lambda ()
          (fib n3 (rampoline-empty-k jumpout))))))))

(define rampoline
  (lambda (th1 th2 th3)
    (rampoline (th1) (th2) (th3))))   


 







