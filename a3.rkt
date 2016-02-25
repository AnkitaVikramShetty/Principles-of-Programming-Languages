#lang racket
(print-as-expression #f)
(require racket/trace)

;; Interpreter 1: Value-of

(define value-of
  (lambda (exp env)
    (match exp

      ;;zero 
      [`(zero? ,a)
       (zero? (value-of a env))]

      ;;sub1
      [`(sub1 ,a)
       (sub1 (value-of a env))]

      ;;multiply *
      [`(* ,a ,b)
       (* (value-of a env)
          (value-of b env))]
      
      ;;let - hide
      [`(let ([,var ,exp])
          ,body)
       (value-of body (lambda (id)
                        (if (eq? id var)
                            (value-of exp env)
                            (env id))))]
      ;;boolean true
      [#t #t]

      ;;boolean false
      [#f #f]

      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (env var )]

      ;;lambda abstraction 
      [`(lambda (,id) ,body)
       (lambda (arg)
         (value-of body (lambda (var )
                          (if (eq? id var )
                              arg
                              (env var )))))] 
      ;;application
      [`(,rator ,rand) ((value-of rator env) (value-of rand env))]

      ;;if condition
      [`(if ,test ,conseq ,alt)
       (if (value-of test env)
           (value-of conseq env)
           (value-of alt env))]
      [`(set! ,id ,expr) (value-of expr ((lambda (x) x) expr))]
      
      [`(begin2 ,op1 ,op2) (begin (value-of op1 env) (value-of op2 env))] 
      
      
      )))


;; Interpreter 2 - value-of-fn

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
      [`(* ,a ,b)
       (* (value-of-fn a env)
          (value-of-fn b env))]
      
      ;;let - hide
      [`(let ([,var ,exp])
          ,body)
       (value-of-fn body (lambda (id)
                        (if (eq? id var)
                            (value-of-fn exp env)
                            (env id))))]
      ;;boolean true
      [#t #t]

      ;;boolean false
      [#f #f]

      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (apply-env-fn env var )]

      ;;lambda abstraction 
      [`(lambda (,id) ,body)
       (lambda (arg)
         (value-of-fn body (extend-env-fn id arg env)))]
      
      ;;application
      [`(,rator ,rand) ((value-of-fn rator env) (value-of-fn rand env))]

      ;;if condition
      [`(if ,test ,conseq ,alt)
       (if (value-of-fn test env)
           (value-of-fn conseq env)
           (value-of-fn alt env))]
      
      )))

(define empty-env-fn
  (lambda()
    (lambda (var ) (error "unbound variable ˜s" var ))))

(define extend-env-fn
  (lambda (id arg env)
    (lambda (var )
       (if (eq? id var )
           arg
           (env var )))))

(define apply-env-fn
  (lambda (env var)
    (env var))) 

;; Interpreter 4 - value-of-ds

(define value-of-ds
  (lambda (exp env)
    (match exp

      ;;zero 
      [`(zero? ,a)
       (zero? (value-of-ds a env))]

      ;;sub1
      [`(sub1 ,a)
       (sub1 (value-of-ds a env))]

      ;;multiply *
      [`(* ,a ,b)
       (* (value-of-ds a env)
          (value-of-ds b env))]
      
      ;;let - hide
      [`(let ([,var ,exp])
          ,body) (value-of-ds body (extend-env-ds 
                          var 
                          (value-of-ds exp env) env)) ]

      ;; var is the id here, (recursion is the arg) and env is env in the call

      ;;boolean true
      [#t #t]

      ;;boolean false
      [#f #f]

      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (apply-env-ds env var )]

      ;;lambda abstraction 
      [`(lambda (,id) ,body)
       (lambda (arg)
         (value-of-ds body (extend-env-ds id arg env)))]
      
      ;;application
      [`(,rator ,rand) ((value-of-ds rator env) (value-of-ds rand env))]

      ;;if condition
      [`(if ,test ,conseq ,alt)
       (if (value-of-ds test env)
           (value-of-ds conseq env)
           (value-of-ds alt env))]

      )))
 
(define apply-env-ds
  (lambda (env var)
    (match env
      [`(extend-env-ds ,id ,arg ,env) (if (eqv? id var)
                                          arg
                                          (apply-env-ds env var))]
      [`(empty-env-ds) (error `empty-env "unbound variable ˜s" var )])))

(define empty-env-ds
  (lambda()
   `(empty-env)))
 

(define extend-env-ds
  (lambda (id arg env)
    `(extend-env-ds ,id ,arg ,env)))


;; Interpreter 4 - fo-eulav

(define fo-eulav
  (lambda (exp env)
    (match exp
      ;;number
      [`,n #:when (number? n) n]

      ;;variable
      [`,var #:when (symbol? var) (apply-env-fn env var )]
 
      ;;(n ?orez)
      [`(,a ?orez)
       (zero? (fo-eulav a env))]

      ;;(n 1bus)
      [`(,a 1bus)
       (sub1 (fo-eulav a env))]

      ;;multiply *
      [`(,a ,b *)
       (* (fo-eulav a env)
          (fo-eulav b env))]

      ;;if condition
      [`(,alt ,conseq ,test fi)
       (if (fo-eulav test env)
           (fo-eulav conseq env)
           (fo-eulav alt env))]
       
      ;;boolean true
      [#t #t]

      ;;boolean false
      [#f #f]

      ;;lambda abstraction 
      [`(,body (,id) adbmal)
       (lambda (arg)
         (fo-eulav body (extend-env-fn id arg env)))]
      
      ;;application
      [`(,rand ,rator)
       ((fo-eulav rator env)
        (fo-eulav rand env))]

      )))


;; brainteaser 6

(define value-of-lex
  (lambda (exp env)
    (match exp
      ((? boolean?) exp)
      ((? number?) exp)
      (`(sub1 ,body) (sub1 (value-of-lex body env)))
      (`(zero? ,body) (zero? (value-of-lex body env)))
      (`(* ,n1 ,n2) (* (value-of-lex n1 env) (value-of-lex n2 env)))
      (`(if ,t ,c ,a) (if (value-of-lex t env) (value-of-lex c env) (value-of-lex a env)))
      (`(var ,num) (apply-env-lex env num))
      (`(lambda ,body) (lambda (a) (value-of-lex body (extend-env-lex a env))))
      (`(,rator ,rand) ((value-of-lex rator env) (value-of-lex rand env))))))
 
(define empty-env-lex 
  (lambda () '()))

(define (extend-env-lex a env)
  (list a env)) ;; since env is a list, simply append 5 in the function call to the env i.e. (5 '())


(define (apply-env-lex env num)
  (if (eq? num 0) ;; if num is zero it means the env has just (5 '())
      (car env)  ;; therefore we can get 5 from car of the env list
      (apply-env-lex (cdr env) (sub1 num))))   ;; if num is say 2