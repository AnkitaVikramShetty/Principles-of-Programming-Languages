#lang racket
(require racket/trace)
(print-as-expression #f)

(define empty-k
  (lambda ()
    (let ((once-only #f))
      (lambda (v)
        (if once-only
	    (error 'empty-k "You can only invoke the empty continuation once")
	    (begin (set! once-only #t) v))))))

;; 1.

(define binary-to-decimal
  (lambda (n)
    (cond
      [(null? n) 0]
      [else (+ (car n) (* 2 (binary-to-decimal (cdr n))))])))

;; CPS:
(define binary-to-decimal-cps
  (lambda (n k)
    (cond
      [(null? n) (k 0)]
      [else (binary-to-decimal-cps (cdr n) (lambda (v) (k (+ (car n) (* 2 v)))))])))

;; 2.

(define times
  (lambda (ls)
    (cond
      [(null? ls) 1]
      [(zero? (car ls)) 0]
      [else (* (car ls) (times (cdr ls)))])))

;; CPS:
(define times-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) (k 0)]
      [else (times-cps (cdr ls) (lambda (v) (k (* (car ls) v))))])))

;; 3.

(define times-cps-shortcut
  (lambda (ls k)
    (cond
      [(null? ls) (k 1)]
      [(zero? (car ls)) 0]
      [else (times-cps-shortcut (cdr ls) (lambda (v) (k (* (car ls) v))))])))


;; 4.

(define plus
  (lambda (m)
    (lambda (n)
      (+ m n))))

;; CPS:
(define plus-cps
  (lambda (m k)
    (k (lambda (n k1)
      (k1 (+ m n))))))

;; 5

(define remv-first-9*
  (lambda (ls)
    (cond
      [(null? ls) '()]
      [(pair? (car ls))
       (cond
         [(equal? (car ls) (remv-first-9* (car ls)))
          (cons (car ls) (remv-first-9* (cdr ls)))]
         [else (cons (remv-first-9* (car ls)) (cdr ls))])]
      [(eqv? (car ls) '9) (cdr ls)]
      [else (cons (car ls) (remv-first-9* (cdr ls)))])))

;; CPS:
(define remv-first-9*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k '())]
      [(pair? (car ls))
       (remv-first-9*-cps (car ls) (lambda (v) (if
                                                (equal? (car ls) v)
                                                (remv-first-9*-cps (cdr ls) (lambda (w) (k (cons (car ls) w))))
                                                (remv-first-9*-cps (car ls) (lambda (x) (k (cons x (cdr ls))))))))]
      [(eqv? (car ls) '9) (k (cdr ls))]
      [else (remv-first-9*-cps (cdr ls) (lambda (f) (k (cons (car ls) f))))])))

;; 6

(define count-syms*
  (lambda (ls)
    (cond
      [(null? ls) 0]
      [(pair? (car ls)) (+ (count-syms* (car ls)) (count-syms* (cdr ls)))]
      [(symbol? (car ls)) (add1 (count-syms* (cdr ls)))]
      [else (count-syms* (cdr ls))])))

;; CPS:
(define count-syms*-cps
  (lambda (ls k)
    (cond
      [(null? ls) (k 0)]
      [(pair? (car ls)) (count-syms*-cps (car ls) (lambda (v) (count-syms*-cps (cdr ls) (lambda (w) (k (+ v w))))))]
      [(symbol? (car ls)) (count-syms*-cps (cdr ls) (lambda (t) (k (add1 t))))]
      [else (count-syms*-cps (cdr ls) k)])))

;; 7

(define cons-cell-count
  (lambda (ls)
    (cond
      [(pair? ls)
       (add1 (+ (cons-cell-count (car ls)) (cons-cell-count (cdr ls))))]
      [else 0])))

;; CPS:
(define cons-cell-count-cps
  (lambda (ls k)
   (cond
      [(pair? ls)
       (add1 (cons-cell-count-cps (car ls) (lambda (v) (cons-cell-count-cps (cdr ls) (lambda (w) (k (+ v w)))))))]
      [else (k 0)])))

;; 8 

(define find 
  (lambda (u s)
    (let ((pr (assv u s)))
      (if pr (find (cdr pr) s) u))))

;; CPS:
(define find-cps 
  (lambda (u s k)
   (let ((pr (assv u s)))
      (if pr (find-cps (cdr pr) s k) (k u)))))
 

;; 9

(define ack
  (lambda (m n)
    (cond
      [(zero? m) (add1 n)]
      [(zero? n) (ack (sub1 m) 1)]
      [else (ack (sub1 m)
                 (ack m (sub1 n)))])))

;; CPS:
(define ack-cps
  (lambda (m n k)
    (cond
      [(zero? m) (k (add1 n))] 
      [(zero? n) (ack-cps (sub1 m) 1 k)]
      [else (ack-cps m (sub1 n) (lambda (v) (ack-cps (sub1 m) v k)))])))

;; 10

(define fib
  (lambda (n)
    ((lambda (fib) 
       (fib fib n))
     (lambda (fib n)
       (cond
	 [(zero? n) 0]
	 [(= 1 n) 1]
	 [else (+ (fib fib (sub1 n)) (fib fib (sub1 (sub1 n))))])))))

;; CPS:
(define fib-cps
  (lambda (n k)
    ((lambda (fib k)
       (fib fib n k)) (lambda (fib n k)
                        (cond
                          [(zero? n) (k 0)]
                          [(= 1 n) (k 1)]
                          [else (fib fib (sub1 n) (lambda (v)
                                                    (fib fib (sub1 (sub1 n))
                                                         (lambda (w)
                                                           (k (+ v w))))))])) k)))

;; 11

(define unfold
  (lambda (p f g seed)
    ((lambda (h)
       ((h h) seed '()))
     (lambda (h)
       (lambda (seed ans)
         (if (p seed)
             ans
             ((h h) (g seed) (cons (f seed) ans))))))))

;; CPS:
(define unfold-cps   
  (lambda (p f g seed k1)
    ((lambda (h k2)
       (h h (lambda (v) (v seed '() k2)))) 
     (lambda (h k3)
       (k3 (lambda (seed ans k4)
             (p seed (lambda (b) (if b
                                     (k4 ans)
                                     (h h (lambda (o) (g seed (lambda (p) (f seed (lambda (m) (o p (cons m ans) k4)))))))))))))
  k1)))

(define null?-cps
    (lambda (ls k)
      (k (null? ls))))

 (define car-cps
    (lambda (pr k)
      (k (car pr))))

 (define cdr-cps
    (lambda (pr k)
      (k (cdr pr))))

;; 12

(define empty-s
  (lambda ()
    '()))
 
(define unify
  (lambda (u v s)
    (cond
      ((eqv? u v) s)
      ((number? u) `((,u . ,v) . ,s))
      ((number? v) (unify v u s))
      ((pair? u)
       (if (pair? v)
	   (let ((s (unify (find (car u) s) (find (car v) s) s)))
             (if s (unify (find (cdr u) s) (find (cdr v) s) s) #f))
	   #f))
      (else #f))))

;; CPS:
(define unify-cps
  (lambda (u v s k)
    (cond
      ((eqv? u v) (k s))
      ((number? u) (k `((,u . ,v) . ,s)))
      ((number? v) (unify-cps v u s k))
      ((pair? u)
       (if (pair? v) 
           (find-cps (car u) s (lambda (l) (find-cps (car v) s (lambda (w) (unify-cps l w s (lambda (s) 
                                                                                      (if s (find-cps (cdr u) s (lambda (a) (find-cps (cdr v) s (lambda (b) (unify-cps a b s k))))) (k #f))))))))
	  (k #f)))
      (else (k #f)))))


;;13.

(define M
  (lambda (f)
    (lambda (ls)
      (cond
        ((null? ls) '())
        (else (cons (f (car ls)) ((M f) (cdr ls))))))))

;; CPS:
(define M-cps
  (lambda (f k)
    (k (lambda (ls k)
      (cond
        ((null? ls) (k '()))
          (else (f (car ls)
                    (lambda (v)
                      (M-cps f (lambda (w)
                                 (w (cdr ls)
                                       (lambda (z)
                                         (k (cons v z))))))))))))))

;; 14

(define use-of-M
  ((M (lambda (n) (add1 n))) '(1 2 3 4 5)))

;; CPS:
(define use-of-M-cps
  ((M-cps (lambda (n k) (k (add1 n))) (empty-k)) '(1 2 3 4 5) (empty-k)))

;; 15

(define strange
  (lambda (x)
    ((lambda (g) (lambda (x) (g g)))
     (lambda (g) (lambda (x) (g g))))))

;; CPS:
(define strange-cps
  (lambda (x k)
    ((lambda (g k) (k (lambda (x k) (g g k))))
     (lambda (g k) (k (lambda (x k) (g g k)))) k)))

;; 16

(define use-of-strange
  (let ([strange^ (((strange 5) 6) 7)])
    (((strange^ 8) 9) 10)))

;; CPS:
(define use-of-strange-cps
  (strange-cps 5 (lambda (c) c 6 (lambda (b) b 7 (lambda (a) (((a 8 (empty-k)) 9 (empty-k)) 10 (empty-k)))))))

;; 17

(define why
  (lambda (f)
    ((lambda (g)
       (f (lambda (x) ((g g) x))))
     (lambda (g)
       (f (lambda (x) ((g g) x)))))))

;; CPS:
(define why-cps
  (lambda (f-cps k)
    ((lambda (g k) (f-cps (lambda (x k) (g g (lambda (a) (a x k)))) k))
     (lambda (g k) (f-cps (lambda (x k) (g g (lambda (b) (b x k)))) k)) k))) 


(define almost-length
    (lambda (f)
      (lambda (ls)
        (if (null? ls)
            0
            (add1 (f (cdr ls)))))))     

;; CPS:
(define almost-length-cps
    (lambda (f k)
      (k (lambda (ls k)
        (if (null? ls)
            (k 0) 
            (f (cdr ls) (lambda (v) (k (add1 v)))))))))    

