#lang racket 
(require racket/trace)
(require "monads.rkt")

;;------------------------------------------ MAYBE MONAD ----------------------------------------------------

;; 1. ASSV-MAYBE

(define assv-maybe
  (lambda (v ls)
    (cond
      [(null? ls) (fail)]
      [else 
       (bind-maybe (return-maybe (car ls))
                   (lambda (s)
                     (if (eq? v (car s))
                         (return-maybe (cdr s))
                         (assv-maybe v (cdr ls)))))]))) 


;;------------------------------------------ WRITER MONAD ----------------------------------------------------

;; 2. PARTITION-WRITER

(define partition-writer
  (lambda (pred l)
    (cond
      [(null? l) (return-writer '())]
      [(pred (car l)) (bind-writer (tell-writer (car l)) (lambda (_) (partition-writer pred (cdr l))))]
      [else (bind-writer (partition-writer pred (cdr l)) (lambda (d) (return-writer (cons (car l) d))))]))) 

;; 3. POWERXPARTIALS
(define power
  (lambda (x n)
    (cond
      [(zero? n) 1]
      [(= n 1) x]
      [(odd? n) (* x (power x (sub1 n)))]
      [(even? n) (let ((nhalf (/ n 2)))
                   (let ((y (power x nhalf)))
                     (* y y)))])))

(define powerXpartials
  (lambda (x n)
    (cond
      [(zero? n) (return-writer 0)]
      [(= n 1) (return-writer x)]
      [(odd? n) (bind-writer (powerXpartials x (sub1 n)) (lambda (d) 
                                                           (bind-writer (tell-writer d) (lambda (_) (return-writer (* x d))))))] 
      [(even? n) (let ((nhalf (/ n 2)))
                   (bind-writer (powerXpartials x nhalf)
                                (lambda (d) 
                                  (bind-writer (tell-writer d) (lambda (_) (return-writer (* d d)))))))]))) 

(define gcd
  (lambda (a b)
    (cond
      ((eqv? a b) a)
      ((> a b) (gcd b a))
      (else (gcd a (- b a))))))

(define gcd-writer
  (lambda (a b)
    (cond
      ((eqv? a b) (return-writer a))
      ((> a b) (bind-writer (tell-writer `(swapping ,a and ,b)) (lambda (x) (gcd-writer b a))))
      (else (bind-writer (gcd-writer a (- b a)) (lambda (x) (return-writer x)))))))   
 
;;------------------------------------------ STATE MONAD ----------------------------------------------------

;; 4. REPLACE-WITH-COUNT

(define replace-with-count
  (lambda (x ls)
    (cond
      [(null? ls) (return-state ls)]
      [(pair? (car ls))  (do bind-state
                           (a <- (replace-with-count x (car ls)))
                           (d <- (replace-with-count x (cdr ls)))
                           (return-state (cons a d)))]
      [(equal? x (car ls)) (do bind-state
                             (s <- get-state)
                             (put-state (add1 s))
                             (r <- (replace-with-count x (cdr ls)))
                             (return-state (cons s r)))]  
      [else  (do bind-state
               ;(s <- get-state)
               (r <- (replace-with-count x (cdr ls)))
               (return-state (cons (car ls) r)))])))

;;------------------------------------------ MIXED MONADS PROBLEMS ----------------------------------------------------

(define traverse
    (lambda (return bind f)
      (letrec
        ((trav
           (lambda (tree)
             (cond
               [(pair? tree)
                (do bind
                  (a <- (trav (car tree)))
                  (d <- (trav (cdr tree)))
                  (return (cons a d)))]
               [else (f tree)]))))
        trav)))

;; 5. RECIPROCAL

(define reciprocal
  (lambda (n)
    (cond
      [(zero? n) (fail)]
      [else (return-maybe (/ 1 n))])))

;; TRAVERSE-RECIPROCAL
 
(define traverse-reciprocal
    (traverse return-maybe bind-maybe reciprocal))

;; 6. HALVE

(define halve
  (lambda (n)
    (cond
      [(even? n) (return-writer (/ n 2))]
      [else (bind-writer (tell-writer n) (lambda (d) (return-writer n)))])))

;; TRAVERSE-HALVE

(define traverse-halve
    (traverse return-writer bind-writer halve))

;; 7. STATE/SUM

(define state/sum
  (lambda (n)
    (do bind-state
      (s <- get-state)
      (put-state (+ s n))
      (return-state s))))

;; TRAVERSE-STATE/SUM

 (define traverse-state/sum
    (traverse return-state bind-state state/sum))

;;------------------------------------------ BRAINTEASER CONTINUATION-MONAD ----------------------------------------------------

(define fact-5
    '((lambda (f)
        ((f f) 5))
      (lambda (f)
        (lambda (n)
          (if (zero? n)
            1
            (* n ((f f) (sub1 n))))))))

(define capture-fun
    '(* 3 (capture q (* 2 (return q 4)))))

;; CPS MONAD INTERPRETER

(define value-of-cps
  (lambda (expr env)
    (match expr
      [(? number?) (return-cont expr)]
      [(? boolean?) (return-cont expr)]       
      [(? symbol?) (return-cont (apply-env env expr))]
      [`(* ,x1 ,x2) (bind-cont (value-of-cps x1 env)
                               (lambda (v)
                                 (bind-cont (value-of-cps x2 env)
                                            (lambda (w)
                                              (return-cont (* v w))))))]
      [`(sub1 ,x) (do bind-cont 
                    (r <- (value-of-cps x env))
                    (return-cont (sub1 r)))]
      [`(zero? ,x) (do bind-cont 
                    (r <- (value-of-cps x env))
                    (return-cont (zero? r)))]
      [`(if ,test ,conseq ,alt) (do bind-cont 
                                  (t <- (value-of-cps test env))
                                  (if t
                                      (value-of-cps conseq env)
                                      (value-of-cps alt env)))]
      [`(capture ,k-id ,body) (callcc (lambda (k)
                                        (value-of-cps body (extend-env k-id k env))))]
      [`(return ,k-exp ,v-exp) (do bind-cont
                                 (lhs <- (value-of-cps k-exp env))
                                 (rhs <- (value-of-cps v-exp env))
                                 (lhs rhs))]
      [`(lambda (,id) ,body) (return-cont (closure id body env))]
      [`(,rator ,rand) (do bind-cont
                         (lhs <- (value-of-cps rator env))
                         (rhs <- (value-of-cps rand env))
                         (apply-proc lhs rhs))])))

;;empty-env, apply-env, extend-env, closure, and apply-proc
(define apply-proc
  (lambda (lhs rhs)
     (lhs rhs)))

(define empty-env
  (lambda ()
    (lambda (var) (error "unbound variable ˜s" var))))

(define extend-env
  (lambda (id arg env)
    (lambda (var)
       (if (eq? id var)
           arg
           (env var)))))

(define apply-env
  (lambda (env var)
    (env var)))

(define closure
  (lambda (id body env)
    (lambda (arg)
         (value-of-cps body (extend-env id arg env)))))

(define apply-closure
  (lambda (lhs rhs)
    (lhs rhs)))

(define quine? 
  (lambda (x)
    (equal? x (eval x))))