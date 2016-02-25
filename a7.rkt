#lang racket
(require racket/trace)
(print-as-expression #f)

;-------------------------------------------- PART I - CALL/CC -----------------------------------------------------------------------
(define last-non-zero
  (lambda (ls)
    (call/cc
      (lambda (k)
        (letrec
          ((last-non-zero1
            (lambda (ls1)
               (cond
                 [(null? ls1) '()]
                 [(zero? (car ls1)) (k (last-non-zero1 (cdr ls1)))] 
                 [else  (cons (car ls1) (last-non-zero1 (cdr ls1)))])
              )))
          (last-non-zero1 ls))))))

;-------------------------------------------- PART II - LEX -----------------------------------------------------------------------
(define calculate_lex_addr
  (lambda (acc y)
    (cond
      ((null? acc) 0)
      ((eqv? y (car acc)) 0)
      (else (add1 (calculate_lex_addr (cdr acc) y))))))

(define lex
  (lambda (expr acc)
    (match expr
      ;;zero
      ;;STEP 2
      ;;[`(zero? ,a) `(zero? ,(lex a acc))] replaced by
      [`(zero? ,nexp) `(zero ,(lex nexp acc))]
      
      ;;number
      [`,n #:when (number? n) `(const ,n)]
     
      ;;sub1
      [`(sub1 ,a) `(sub1 ,(lex a acc))]

      ;;multiply *
      ;;STEP 3
      ;; [`(* ,a ,b) `(* ,(lex a acc) ,(lex b acc))] replaced by
      [`(* ,nexp1 ,nexp2) `(mult ,(lex nexp1 acc) ,(lex nexp2 acc))]
      
      ;;let
      [`(let ((,var ,exp)) ,body) `(let ,(lex exp acc) ,(lex body (cons var acc)))] 

      ;;symbol
      [`,y #:when (symbol? y) `(var ,(calculate_lex_addr acc y))]

      ;;lambda abstraction
      [`(lambda (,x) ,e) `(lambda ,(lex e (cons x acc)))]

      ;;application
      ;;STEP 1
      ;;[`(,e1 ,e2) (list `,(lex e1 acc) `,(lex e2 acc))] replaced by:
      [`(,rator ,rand) `(app ,(lex rator acc) ,(lex rand acc))]
      
      ;;if condition
      [`(if ,test ,conseq ,alt) `(if ,(lex test acc) ,(lex conseq acc) ,(lex alt acc))]

      ;;capture
      [`(capture ,x ,body) `(capture ,(call/cc (lambda (k) (lex body (cons x acc)))))] 

      ;;return
      [`(return ,k-exp ,v-exp) `(return ,(lex k-exp acc) ,(lex v-exp acc))])))  


;-------------------------------------------- PART III - INTERPRETER -----------------------------------------------------------------------
(define value-of-cps
  (lambda (expr env k)  
    (match expr
      [`(const ,expr) (apply-k k expr)]
      [`(mult ,x1 ,x2) (value-of-cps x1 env (outer-k-mult x2 env k))] 
      [`(sub1 ,x) (value-of-cps x env (constructor-sub1 k))]
      [`(zero ,x) (value-of-cps x env (constructor-zero k))]   
      [`(if ,test ,conseq ,alt) (value-of-cps test env (constructor-if conseq alt env k))]
      [`(capture ,body) (value-of-cps body (extend-env k env) k)]     
      [`(return ,k-exp ,v-exp) (value-of-cps k-exp env (constructor-return v-exp env))]   
      [`(let ,e ,body) (value-of-cps e env (constructor-let body env k))]   
      [`(var ,expr) (apply-env env expr k)] 
      [`(lambda ,body) (apply-k k (closure body env))]
      [`(app ,rator ,rand) (value-of-cps rator env (outer-k-app rand env k))])))     
  
(define empty-env
  (lambda ()
    `(empty-env)))

 (define apply-env
  (lambda (env y k) 
    (match env
      [`(extend-env ,a^ ,env^) (if (zero? y) (apply-k k a^) (apply-env env^ (sub1 y) k))] 
      [`(empty-env) (error 'value-of "unbound identifier")]))) 

(define extend-env
  (lambda (a^ env^)
    `(extend-env ,a^ ,env^)))

(define apply-closure
  (lambda (lhs a k)
   (match lhs
     [`(closure ,body ,env) (value-of-cps body (extend-env a env) k)]))) 

(define apply-k
  (lambda (k v)
    (match k
      [`(empty-k) v]
      [`(inner-k-mult ,v^ ,k^) (apply-k k^ (* v^ v))]
      [`(outer-k-mult ,x2^ ,env^ ,k^) (value-of-cps x2^ env^ (inner-k-mult v k^))]
      [`(constructor-sub1 ,k^) (apply-k k^ (sub1 v))]
      [`(constructor-zero ,k^) (apply-k k^ (zero? v))]
      [`(constructor-if ,conseq^ ,alt^ ,env^ ,k^) (if v (value-of-cps conseq^ env^ k^) (value-of-cps alt^ env^ k^))]
      [`(constructor-return ,v-exp^ ,env^) (value-of-cps v-exp^ env^ v)]
      [`(inner-k-app ,c^ ,k^) (apply-closure c^ v k^)]
      [`(outer-k-app ,rand^ ,env^ ,k^) (value-of-cps rand^ env^ (inner-k-app v k^))]
      [`(constructor-let ,body^ ,env^ ,k^) (value-of-cps body^ (extend-env v env^) k^)])))   

(define closure
  (lambda (body env)
   `(closure ,body ,env)))

(define inner-k-mult
  (lambda (v^ k^)
    `(inner-k-mult ,v^ ,k^)))

(define outer-k-mult
  (lambda (x2^ env^ k^)
    `(outer-k-mult ,x2^ ,env^ ,k^)))

(define constructor-sub1
  (lambda (k^)
    `(constructor-sub1 ,k^)))

(define constructor-zero
  (lambda (k^)
    `(constructor-zero ,k^)))

(define constructor-if
  (lambda (conseq^ alt^ env^ k^)
    `(constructor-if ,conseq^ ,alt^ ,env^ ,k^)))

(define constructor-return
  (lambda (v-exp^ env^)
    `(constructor-return ,v-exp^ ,env^)))

(define inner-k-app
  (lambda (c^ k^)
    `(inner-k-app ,c^ ,k^)))

(define outer-k-app
  (lambda (rand^ env^ k^)
    `(outer-k-app ,rand^ ,env^ ,k^)))

(define constructor-let
  (lambda (body^ env^ k^)
    `(constructor-let ,body^ ,env^ ,k^)))

(define empty-k
  (lambda ()
    `(empty-k)))

;-------------------------------------------- BRAINTEASER -----------------------------------------------------------------------

(define car$ car)
 
(define cdr$
  (lambda ($) (force (cdr $))))

(define inf-1s (cons$ 1 inf-1s))

(define take$
  (lambda (n $)
    (cond
      ((zero? n) '())
      (else (cons (car$ $) 
              (let ((n- (sub1 n)))
                (cond
                  ((zero? n-) '())
                  (else (take$ n- (cdr$ $))))))))))

(define-syntax cons$
  (syntax-rules ()
    ((cons$ x y) (cons x (delay y)))))

(define adding3
  (lambda (a b c)
    (let ((sum (+ a b c)))
      (cons$ sum (adding3 b c sum)))))

(define trib$
  (cons$ 0 (cons$ 1 (cons$ 1 (adding3 0 1 1)))))
