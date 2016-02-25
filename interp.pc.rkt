;#lang racket
;(require "parenthec.rkt") 
  
;; STEP 10 

;; Comment out the lines #lang racket, (require “parentheC.rkt”),
;; and your invocation of main if you added it to your file.
;; And save a copy of this file named interp.pc.

;(print-as-expression #f)
;(require racket/trace) 
 
;; GLOBAL REGISTERS

(define-registers k v expression env lhs a y)

;; PROGRAM COUNTER DEFINITION

(define-program-counter pc)
  
(define-union expr
  (const cexp)
  (var n)
  (if test conseq alt)
  (mult nexp1 nexp2)
  (sub1 nexp)
  (zero nexp)
  (capture body)
  (return kexp vexp)
  (let exp1 body)              
  (lambda body)
  (app rator rand))
 
(define-label value-of-cps
  (union-case expression expr  
      [(const expression) (begin (set! k k) (set! v expression) (set! pc apply-k) )]
      [(mult x1 x2) (begin (set! k (cont-expr_outer-k-mult x2 env k)) (set! expression x1) (set! env env) (set! pc value-of-cps))]   
      [(sub1 x) (begin (set! k (cont-expr_constructor-sub1 k)) (set! expression x) (set! env env) (set! pc value-of-cps))]
      [(zero x) (begin (set! k (cont-expr_constructor-zero k)) (set! expression x) (set! env env) (set! pc value-of-cps))]    
      [(if test conseq alt) (begin (set! k (cont-expr_constructor-if conseq alt env k)) (set! expression test) (set! env env) (set! pc value-of-cps))]
      [(capture body) (begin (set! env (env-expr_extend-env k env)) (set! expression body) (set! k k) (set! pc value-of-cps))]     
      [(return k-exp v-exp) (begin (set! k (cont-expr_constructor-return v-exp env)) (set! expression k-exp) (set! env env) (set! pc value-of-cps))]   
      [(let e body) (begin (set! k (cont-expr_constructor-let body env k)) (set! expression e) (set! env env) (set! pc value-of-cps))]   
      [(var expression) (begin (set! env env) (set! y expression) (set! k k) (set! pc apply-env) )] 
      [(lambda body) (begin (set! v (closure-expr_closure body env)) (set! k k) (set! pc apply-k) )]
      [(app rator rand) (begin (set! k (cont-expr_outer-k-app rand env k)) (set! expression rator) (set! env env) (set! pc value-of-cps))]))        

;; CLOSURE CONSTRUCTOR
#;(define closure
  (lambda (body env)
   `(closure ,body ,env)))

(define-union closure-expr
  (closure body^ env^))

(define-label apply-closure
  (union-case lhs closure-expr
     [(closure body^ env^) (begin (set! env (env-expr_extend-env a env^)) (set! expression body^) (set! k k) (set! pc value-of-cps))]))

;; ENVIRONMENT CONSTRUCTORS
#;(define empty-env
  (lambda ()
    `(empty-env)))

#;(define extend-env
  (lambda (a^ env^)
    `(extend-env ,a^ ,env^)))

(define-union env-expr
  (empty-env)
  (extend-env a^ env^))

(define-label apply-env
  (union-case env env-expr
      [(extend-env a^ env^) (if (zero? y) (begin (set! k k) (set! v a^) (set! pc apply-k) ) (begin (set! env env^) (set! k k) (set! y (sub1 y)) (set! pc apply-env) ))] 
      [(empty-env) (error 'value-of-cps "unbound identifier")]))     

;; CONTINUATION CONSTRUCTORS
#;(define inner-k-mult
  (lambda (v^ k^)
    `(inner-k-mult ,v^ ,k^)))

#;(define outer-k-mult
  (lambda (x2^ env^ k^)
    `(outer-k-mult ,x2^ ,env^ ,k^)))

#;(define constructor-sub1
  (lambda (k^)
    `(constructor-sub1 ,k^)))

#;(define constructor-zero
  (lambda (k^)
    `(constructor-zero ,k^)))

#;(define constructor-if
  (lambda (conseq^ alt^ env^ k^)
    `(constructor-if ,conseq^ ,alt^ ,env^ ,k^)))

#;(define constructor-return
  (lambda (v-exp^ env^)
    `(constructor-return ,v-exp^ ,env^)))

#;(define inner-k-app
  (lambda (c^ k^)
    `(inner-k-app ,c^ ,k^)))

#;(define outer-k-app
  (lambda (rand^ env^ k^)
    `(outer-k-app ,rand^ ,env^ ,k^)))

#;(define constructor-let
  (lambda (body^ env^ k^)
    `(constructor-let ,body^ ,env^ ,k^)))

#;(define empty-k
  (lambda ()
    `(empty-k)))

(define-union cont-expr
  (inner-k-mult v^ k^)
  (outer-k-mult x2^ env^ k^)
  (constructor-sub1 k^)
  (constructor-zero k^)
  (constructor-if conseq^ alt^ env^ k^)
  (constructor-return v-exp^ env^)
  (inner-k-app c^ k^)
  (outer-k-app rand^ env^ k^)
  (constructor-let body^ env^ k^)
  (empty-k jumpout))

(define-label apply-k
  (union-case k cont-expr
      [(empty-k jumpout) (dismount-trampoline jumpout)]
      [(inner-k-mult v^ k^) (begin (set! v (* v^ v)) (set! k k^) (set! pc apply-k) )] 
      [(outer-k-mult x2^ env^ k^) (begin (set! k (cont-expr_inner-k-mult v k^)) (set! expression x2^) (set! env env^) (set! pc value-of-cps))]
      [(constructor-sub1 k^) (begin (set! k k^) (set! v (sub1 v)) (set! pc apply-k) )]
      [(constructor-zero k^) (begin (set! k k^) (set! v (zero? v)) (set! pc apply-k) )]
      [(constructor-if conseq^ alt^ env^ k^) (if v (begin (set! expression conseq^) (set! env env^) (set! k k^) (set! pc value-of-cps)) (begin (set! expression alt^) (set! env env^) (set! k k^) (set! pc value-of-cps)))]
      [(constructor-return v-exp^ env^) (begin (set! expression v-exp^) (set! env env^) (set! k v) (set! pc value-of-cps))]
      [(inner-k-app c^ k^) (begin (set! lhs c^) (set! a v) (set! k k^) (set! pc apply-closure) )]
      [(outer-k-app rand^ env^ k^) (begin (set! k (cont-expr_inner-k-app v k^)) (set! expression rand^) (set! env env^) (set! pc value-of-cps))]
      [(constructor-let body^ env^ k^) (begin (set! env (env-expr_extend-env v env^)) (set! expression body^) (set! k k^) (set! pc value-of-cps))]))

;; MAIN 

(define-label main 
  (begin
    (set! expression (expr_let 
               (expr_lambda
                (expr_lambda 
                 (expr_if
                  (expr_zero (expr_var 0))
                  (expr_const 1)
                  (expr_mult (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_sub1 (expr_var 0)))))))
               (expr_mult
                (expr_capture
                 (expr_app
                  (expr_app (expr_var 1) (expr_var 1))
                  (expr_return (expr_var 0) (expr_app (expr_app (expr_var 1) (expr_var 1)) (expr_const 4)))))
                (expr_const 5))))
    (set! env (env-expr_empty-env))
    (set! pc value-of-cps)
    (mount-trampoline cont-expr_empty-k k pc)
    (printf "Fact 5: ~s\n" v)))
  
     
     
    
