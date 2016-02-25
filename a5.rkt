#lang racket
(require racket/trace)
(print-as-expression #f)

;; Functional Helpers
(define extend-env
  (lambda (x arg env)
    (lambda (y) (if (eq? y x) arg (apply-env env y)))))

(define apply-env
  (lambda (env y)
    (env y)))

(define empty-env
  (lambda ()
    (lambda (y) (error "unbound variable ~s" y))))  

(define apply-closure
  (lambda (lhs rhs)
    (lhs rhs)))

(define unbox/need
  (lambda (b)
    (let ([val ((unbox b))]) ;;return thunk
      (set-box! b (lambda () val)) ;;update box
  val)))


;; CONVENTIONS:

#|
_________________________________________
call-by-value	    |   val-of-cbv       |
call-by-reference   |	val-of-cbr       |
call-by-name        |	val-of-cbname    |
call-by-need        |	val-of-cbneed    |
------------------------------------------
|#


;; CALL BY VALUE INTERPRETER
 

(define val-of-cbv
  (lambda (exp env)
    (match exp
      [`(quote ,v) v]
      [`,b #:when (boolean? b) b]
      [`(zero? ,n) (zero? (val-of-cbv n env))]
      [`(sub1 ,n) (sub1 (val-of-cbv n env))]
      [`(* ,n1 ,n2) (* (val-of-cbv n1 env) (val-of-cbv n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbv test env)
                                  (val-of-cbv conseq env)
                                  (val-of-cbv alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbv e1 env) (val-of-cbv e2 env))]
      [`(random ,n) (random (val-of-cbv n env))]
      [`(add1 ,n) (add1 (val-of-cbv n env))]
      [`(null? ,ls) (null? (val-of-cbv ls env))]
      [`(let ((,var ,val)) ,body) (let ((u (box (val-of-cbv val env)))) (val-of-cbv body (extend-env var u env)))] 
      [`(cons^ ,a ,d) (cons (box (lambda () (val-of-cbv a env))) (box (lambda () (val-of-cbv d env))))]
      [`(car^ ,ls) (unbox/need (car (val-of-cbv ls env)))]
      [`(cdr^ ,ls) (unbox/need (cdr (val-of-cbv ls env)))]
      [`(cons ,a ,d) (cons (val-of-cbv a env) (val-of-cbv d env))]
      [`(car ,ls) (car (val-of-cbv ls env))]
      [`(cdr ,ls) (car (val-of-cbv ls env))] 
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (closure-val-of-cbv x body env)]
      [`(,rator ,y) #:when (symbol? y) ((val-of-cbv rator env) (box (unbox (apply-env env y))))]
      [`(,rator ,rand) (apply-closure (val-of-cbv rator env) (box (val-of-cbv rand env)))]
      [`(set! ,y ,rhs) (let ((v-rhs (val-of-cbv rhs env))) (set-box! (apply-env env y) v-rhs))]
      [`,n #:when (number? n)  n]
   )))

;; Closure function for Call by value:

(define closure-val-of-cbv
  (lambda (x body env)
    (lambda (arg) (val-of-cbv body (extend-env x arg env)))))

;; CALL BY REFERENCE INTERPRETER
 
(define val-of-cbr
  (lambda (exp env)
    (match exp
        [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbr n env))]
      [`(sub1 ,n) (sub1 (val-of-cbr n env))]
      [`(* ,n1 ,n2) (* (val-of-cbr n1 env) (val-of-cbr n2 env))]
      [`(if ,test ,conseq ,alt) (if (val-of-cbr test env)
                                  (val-of-cbr conseq env)
                                  (val-of-cbr alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbr e1 env) (val-of-cbr e2 env))]
      [`(random ,n) (random (val-of-cbr n env))]
      [`,y #:when (symbol? y) (unbox (env y))]
      [`(lambda (,x) ,body) (closure-val-of-cbr x body env)]
      [`(,rator ,y) #:when (symbol? y) ((val-of-cbr rator env) (apply-env env y))]
      [`(,rator ,rand) (apply-closure (val-of-cbr rator env) (box (val-of-cbr rand env)))]
      [`(set! ,y ,rhs) (let ((v-rhs (val-of-cbr rhs env))) (set-box! (apply-env env y) v-rhs))]))) 

;; Closure function for Call by reference:

(define closure-val-of-cbr
  (lambda (x body env)
    (lambda (arg) (val-of-cbr body (extend-env x arg env)))))

;; CALL BY NAME INTERPRETER

(define val-of-cbname
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) (zero? (val-of-cbname n env))]
      [`(sub1 ,n) (sub1 (val-of-cbname n env))]
      [`(* ,n1 ,n2) (* (val-of-cbname n1 env) (val-of-cbname n2 env))] 
      [`(if ,test ,conseq ,alt) (if (val-of-cbname test env)
                                  (val-of-cbname conseq env)
                                  (val-of-cbname alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbname e1 env) (val-of-cbname e2 env))]
      [`(random ,n) (random (val-of-cbname n env))]
      [`,y #:when (symbol? y) ((unbox (apply-env env y)))] 
      [`(lambda (,x) ,body) (closure-val-of-cbname x body env)]
      [`(,rator ,y) #:when (symbol? y) ((val-of-cbname rator env) (apply-env env y))]
      [`(,rator ,rand) (apply-closure (val-of-cbname rator env) (box (lambda () (val-of-cbname rand env))))]
      [`(set! ,y ,rhs) (let ((v-rhs (val-of-cbname rhs env))) (set-box! (apply-env env y) v-rhs))]))) 

;; Closure function for Call by name:

(define closure-val-of-cbname
  (lambda (x body env)
    (lambda (arg) (val-of-cbname body (extend-env x arg env)))))

;; CALL BY NEED INTERPRETER

(define val-of-cbneed
  (lambda (exp env)
    (match exp
      [`,b #:when (boolean? b) b]
      [`,n #:when (number? n)  n]
      [`(zero? ,n) #:when (number? n) (zero? (val-of-cbneed n env))]
      [`(sub1 ,n) (sub1 (val-of-cbneed n env))]
      [`(* ,n1 ,n2) (* (val-of-cbneed n1 env) (val-of-cbneed n2 env))] 
      [`(if ,test ,conseq ,alt) (if (val-of-cbneed test env)
                                  (val-of-cbneed conseq env)
                                  (val-of-cbneed alt env))]
      [`(begin2 ,e1 ,e2) (begin (val-of-cbneed e1 env) (val-of-cbneed e2 env))]
      [`(random ,n) (random (val-of-cbneed n env))]
      [`,y #:when (symbol? y) (unbox (apply-env env y))]
      [`(lambda (,x) ,body) (closure-val-of-cbneed x body env)]
      [`(,rator ,y) #:when (symbol? y) (let ((b (apply-env env y)))
                                         (let ((v (unbox b)))
                                           (begin (set-box! b (lambda () v)) v)))]
      [`(,rator ,rand) (apply-closure (val-of-cbneed rator env) (box (lambda () (val-of-cbneed rand env))))]
      [`(set! ,y ,rhs) (let ((v-rhs (val-of-cbneed rhs env))) (set-box! (apply-env env y) v-rhs))]))) 


;; Closure function for Call by need:

(define closure-val-of-cbneed
  (lambda (x body env)
    (lambda (arg) (val-of-cbneed body (extend-env x arg env))))) 


