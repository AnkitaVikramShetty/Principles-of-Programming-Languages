#lang racket
(print-as-expression #f)
(require racket/trace)

;; PART I

(define lex
  (lambda (expr acc)
    (match expr
      ;;zero 
      [`(zero? ,a) `(zero? ,(lex a acc))]
      
      ;;number
      [`,n #:when (number? n) `(const ,n)]
     
      ;;sub1
      [`(sub1 ,a) `(sub1 ,(lex a acc))]

      ;;multiply *
      [`(* ,a ,b) `(* ,(lex a acc) ,(lex b acc))]
      
      ;;let
      [`(let ((,var ,exp)) ,body) `(let ,(lex exp acc) ,(lex body (cons var acc)))]

      ;;symbol
      [`,y #:when (symbol? y) `(var ,(calculate_lex_addr acc y))]

      ;;lambda abstraction
      [`(lambda (,x) ,e) `(lambda ,(lex e (cons x acc)))]

      ;;application
      [`(,e1 ,e2) (list `,(lex e1 acc) `,(lex e2 acc))]
      
      ;;if condition
      [`(if ,test ,conseq ,alt) `(if ,(lex test acc) ,(lex conseq acc) ,(lex alt acc))])))

;; helper function for lex calculating the lexical addresss

(define calculate_lex_addr
  (lambda (acc y)
    (cond
      ((null? acc) 0)
      ((eqv? y (car acc)) 0)
      (else (add1 (calculate_lex_addr (cdr acc) y)))))) 


;;------------------------------------------------ END OF PART 1 ---------------------------------------------------------------

;; PART II

;; INTERPRETER 1 (representation independent with respect to closures) : VALUE-OF-FN 

(define value-of-fn
  (lambda (exp env)
    (match exp

      ;;zero 
      [`(zero? ,a)
       (zero? (value-of-fn a env))]

      ;;sub1
      [`(sub1 ,a)
       (sub1 (value-of-fn a env))]

      ;;multiply *
      [`(* ,a ,b) (* (value-of-fn a env) (value-of-fn b env))]
      
      ;;let - hide
      [`(let ([,var ,exp]) ,body) (value-of-fn body (extend-env var (value-of-fn exp env) env))]

      ;;boolean true
      [#t #t]

      ;;boolean false
      [#f #f]

      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (apply-env env var )]

      ;;lambda abstraction 
      [`(lambda (,id) ,body) (closure-fn id body env)]
      
      ;;application
      [`(,rator ,rand) (apply-closure-fn (value-of-fn rator env) (value-of-fn rand env))]

      ;;if condition
      [`(if ,test ,conseq ,alt) (if (value-of-fn test env) (value-of-fn conseq env) (value-of-fn alt env))])))

;; functional representation of closures

(define closure-fn
  (lambda (id body env)
    (lambda (arg)
         (value-of body (extend-env id arg env)))))

(define apply-closure-fn
  (lambda (lhs rhs)
    (lhs rhs)))

;; INTERPRETER 2 (representation independent with respect to closures) : VALUE-OF-DS

(define value-of-ds
  (lambda (exp env)
    (match exp

      ;;zero 
      [`(zero? ,a) (zero? (value-of-ds a env))]

      ;;sub1
      [`(sub1 ,a)
       (sub1 (value-of-ds a env))]

      ;;multiply *
      [`(* ,a ,b)
       (* (value-of-ds a env)
          (value-of-ds b env))]
      
      ;;let - hide
      [`(let ([,var ,exp]) ,body) (value-of-ds body (extend-env var (value-of-ds exp env) env))];(value-of-ds body (extend-env var (value-of-ds exp env) env)) ]

      ;;boolean true
      [#t #t]

      ;;boolean false
      [#f #f]

      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (apply-env env var )]

      ;;lambda abstraction 
      [`(lambda (,id) ,body) (closure-ds id body env)]
      
      ;;application
      [`(,rator ,rand) (apply-closure-ds (value-of-ds rator env) (value-of-ds rand env))]

      ;;if condition
      [`(if ,test ,conseq ,alt) (if (value-of-ds test env) (value-of-ds conseq env) (value-of-ds alt env))])))

;; data-structural representation of closures

(define closure-ds
  (lambda (id body env)
    `(closure-ds ,id ,body ,env)))

(define apply-closure-ds
  (lambda (lhs rhs)
    (match lhs
        [`(closure-ds ,id ,body ,env) (value-of body (extend-env id rhs env))])))

;;------------------------------------------------  END OF PART II  ---------------------------------------------------------------

;; Part III

;; INTERPRETER 3 : USING DYNAMIC SCOPE 

(define value-of-dynamic
  (lambda (exp env)
    (match exp
      ;;null
      [`(null? ,a) (null? (value-of-dynamic a env))]
      
      ;;cons 
      [`(cons ,a ,b) (cons (value-of-dynamic a env) (value-of-dynamic b env))]
      
      ;;car 
      [`(car ,ls) (car (value-of-dynamic ls env))]
      
      ;;cdr
      [`(cdr ,ls) (cdr (value-of-dynamic ls env))]
      
      ;;zero 
      [`(zero? ,a) (zero? (value-of-dynamic a env))]
      
      ;;sub1
      [`(sub1 ,a) (sub1 (value-of-dynamic a env))]
      
      ;;multiply *
      [`(* ,a ,b) (* (value-of-dynamic a env) (value-of-dynamic b env))]
      
      ;;let - hide
      [`(let ([,var ,exp]) ,body) (value-of-dynamic body (extend-env var (value-of-dynamic exp env) env))]
      
      ;;quote
      [`(quote ,v) v]
      
      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (apply-env env var )]

      ;;lambda abstraction 
      [`(lambda (,id) ,body) `(lambda (,id) ,body)]

      ;;application
      [`(,rator ,rand) (match-let ([`(lambda (,x) ,b) (value-of-dynamic rator env)]
                                   [`,a (value-of-dynamic rand env)])
                         (value-of-dynamic b (extend-env x a env)))]
      ;;if condition
      [`(if ,test ,conseq ,alt) (if (value-of-dynamic test env) (value-of-dynamic conseq env) (value-of-dynamic alt env))])))

;;------------------------------------------------  END OF PART III  ---------------------------------------------------------------


;; BRAINTEASER :

;; All functions with respect to functions

(define empty-env-fn
  (lambda ()
    (lambda (var) (error "unbound variable ˜s" var ))))

(define extend-env-fn
  (lambda (id arg env)
    (lambda (var)
       (if (eq? id var)
           arg 
           (env var)))))

(define apply-env-fn
  (lambda (env var)
    (env var)))

;; All functions with respect to data structure

(define apply-env-ds
  (lambda (env var)
    (match env
      [`(extend-env-ds ,id ,arg ,env) (if (eqv? id var) arg (apply-env-ds env var))]
      [`(empty-env-ds) (error `empty-env "unbound variable ˜s" var)])))  

(define empty-env-ds
  (lambda()
   `(empty-env)))

(define extend-env-ds
  (lambda (id arg env)
    `(extend-env-ds ,id ,arg ,env)))

;; Closures with respect to functions

(define closure-fn-ri
  (lambda (x body env extrapar1)   
    (lambda (arg extrapar2) 
      ((extrapar1 (extrapar2 x arg env)) body)))) 

(define apply-closure-fn-ri
  (lambda (lhs rhs extend-env-fn) 
    (lhs rhs extend-env-fn)))

;; Closures with respect to data structure

(define closure-ds-ri
  (lambda (id body env extrapar1)
    `(closure-ds-ri ,id ,body ,env ,extrapar1)))

(define apply-closure-ds-ri
  (lambda (lhs rhs extend-env-ds)
    (match lhs
        [`(closure-ds-ri ,id ,body ,env ,extrapar1) ((extrapar1 (extend-env-ds id rhs env)) body)]))) 
    
;; VALUE-OF-RI
(define value-of-ri
  (lambda (empty-env extend-env apply-env closure-ri apply-closure-ri)
    (letrec ([name (lambda (env)
                     (lambda (exp)
                       (match exp
                         [`(zero? ,a) (zero? ((name env) a))]
                         [`(sub1 ,a) (sub1 ((name env) a))]
                         [`(* ,a ,b) (* ((name env) a) ((name env) b))]
                         [`(let ([,var ,exp]) ,body) ((name (extend-env var ((name env) exp) env)) body)]     
                         [#t #t]
                         [#f #f]
                         [`,n #:when (number? n) n]
                         [`,var #:when (symbol? var) (apply-env env var)]
                         [`(lambda (,id) ,body) (closure-ri id body env name)]   
                         [`(,rator ,rand) (apply-closure-ri ((name env) rator) ((name env) rand) extend-env)]  
                         [`(if ,test ,conseq ,alt) (if ((name env) test) ((name env) conseq) ((name env) alt))])))]) 
      (name (empty-env)))))


