#lang racket
(require racket/trace)
(print-as-expression #f)

;; 1. Complete partial definition of list-ref
;; Input: A list and a position n
;; Output: Symbol at position n
;; Examples:
;; > (list-ref '(a b c) 2)
;; c
;; > (list-ref '(a b c) 0)
;; a

(define list-ref
  (lambda (ls n)
    (letrec
      ((nth-cdr
         (lambda (n)
	   (cond
      [(zero? n) ls]
      [else (cdr (nth-cdr (- n 1)))])     
              )))
      (car (nth-cdr n)))))

;; 2. Union
;; Input: Two lists
;; Output: Union of the lists
;; Examples: 
;; > (union '() '())
;; ()
;; > (union '(x y) '(x z))
;; (x y z)

(define union
  (lambda (ls1 ls2)
    (cond
      [(null? ls2) ls1]
      [else (if (memv (car ls2) ls1)
             (union ls1 (cdr ls2))
             (union (append ls1 (list (car ls2))) (cdr ls2)))])))

;; 3. Extend
;; Examples:
;; > ((extend 1 even?) 0)
;; #t
;; > (filter (extend 1 even?) '(0 1 2 3 4 5))
;; (0 1 2 4)

(define extend
  (lambda (x pred)
    (lambda (result)
      (or (eqv? result x)
          (pred result)))))

;; 4. Walk-symbol
;; Examples:
;; > (walk-symbol 'a '((a . 5)))
;; 5
;; > (walk-symbol 'b '((a . 5) (b . ((c . a))) (c . a)))
;; ((c . a))
;; > (walk-symbol 'd '((a . 5) (b . (1 2)) (c . a) (e . c) (d . e)))
;; 5

(define walk-symbol
  (lambda (x s)
    (let ([walk_temp (assv x s)])
      (cond
        [(if (eqv? #f walk_temp)
             x
             (if (pair? walk_temp)
                 (walk-symbol (cdr walk_temp) s)
                 (cdr walk_temp)))
             ])))) 

#;(Lambda Calculus:
          e ::= x | ( lambda(x) e) | (e e)
   )

;; 5. Lambda to Lumbda
;; Examples:
;; > (lambda->lumbda 'x)
;; x
;; > (lambda->lumbda '(lambda (x) x))
;; (lumbda (x) x)
;; > (lambda->lumbda '(lambda (z) ((lambda (y) (a z)) (h (lambda (x) (h a))))))
;; (lumbda (z) ((lumbda (y) (a z)) (h (lumbda (x) (h a)))))

(define lambda->lumbda
  (lambda (expr)
    (match expr
      [e #;when (symbol? e) expr]
      [`(lambda (,x) ,e)
       `(lumbda (,x) ,(lambda->lumbda e))] 
      [`(,e1 ,e2) `(,(lambda->lumbda e1) ,(lambda->lumbda e2))]))) 

;; 6. Var occurs 
;; > (var-occurs? 'x '(lambda (y) (x z)))
;; #t
;; > (var-occurs? 'x '((z y) x))
;; #t


#;(define var-occurs?*
  (lambda (a expr)
    (match expr
      [(? symbol?) #t]
      [`(lambda (,x) ,e)
       (cond
         [(list? e) (if `(memv ,a ,e) #t #f)]
         [else (if (eqv? a e) #t #f)])]
      [e #:when (not(memv 'lambda e)) (if (memv a e) #t #f)]
      [`(,e1 ,e2) `(,(var-occurs?* e1) ,(var-occurs?* e2))] 
      ))
    )

#;(define var-occurs?
  (lambda (a expr)
    (match expr
      [(? symbol?) #t]
      [`(lambda (,x) ,e)
       (if (list? e)
           (if (memv 'lambda e) (var-occurs? a e) (if (memv a e) #t #f))
           (if (list? e)
               (if (memv a `,e) #t #f)
               (if (eqv? a e) #t #f)))]
      [e #:when (not(memv 'lambda e)) (if (memv a e) #t #f)]
      [`(,e1 ,e2) `(,(var-occurs? e1) ,(var-occurs? e2))]  
      )))



(define var-occurs?
  (lambda (a expr)
    (match expr
      [(? symbol?) #t]
      [`(lambda (,x) ,e)
       (cond
         [(list? e) (if (memv a `,e) #t #f)]
         [else (if (eqv? a e) #t #f)])]
      [e #:when (not(memv 'lambda e)) (if (memv a e) #t #f)]
      [`(,e1 ,e2) `(,(var-occurs? e1) ,(var-occurs? e2))] 
      )))

;; 7. Vars
;; Examples:
;; > (vars '((lambda (y) (x x)) (x y)))
;; (x x x y)
;; > (vars '(lambda (z) ((lambda (y) (a z))
;;                    (h (lambda (x) (h a))))))
;; (a z h h a)

(define vars
  (lambda (expr)
    (match expr
      [(? symbol?) #;when (not (pair? expr))  `(,expr)]       
      [`(lambda (,x) ,e) (vars e)] 
      [`(,e1 ,e2) (append (vars e1) (vars e2))])))

;; 8. Unique vars
;; Examples:
;; > (unique-vars '((lambda (z) (lambda (y) (z y))) x))
;; (z y x)
;; > (unique-vars '((lambda (a) (a b)) ((lambda (c) (a c)) (b a))))
;; (c b a)

(define unique-vars
  (lambda (expr)
    (match expr
      [(? symbol?) #;when (not (pair? expr))  `(,expr)]       
      [`(lambda (,x) ,e) (unique-vars e)] 
      [`(,e1 ,e2) (union (unique-vars e1) (unique-vars e2))])))

;; 9. Var occurs free
;; Examples:
;; > (var-occurs-free? 'x '(lambda (x) (x y)))
;; #f
;; > (var-occurs-free? 'y '(lambda (x) (x y)))
;; #t

(define var-occurs-free?
  (lambda (a expr)
    (match expr
      [(? symbol?) #t]
      [`(lambda (,x) ,e)
        (if (list? e)
             (if (memv 'lambda e) (var-occurs-free? a e) (if (and (eqv? a x) `(memv ,x ,e)) #t #f)) ;memv a e
             (if (var-occurs? `,a `,expr)
                 (if (and (eqv? a x) `(memv ,x ,e)) #f #t)
                 #f))] 
      [e #:when (not(memv 'lambda e)) (if (memv a e) #f #t)]
      [`(,e1 ,e2) `(or ,(var-occurs-free? e1) ,(var-occurs-free? e2))] 
      )))

;; 10. Var occurs bound
;; Examples:
;; > (var-occurs-bound? 'x '((lambda (x) (x x)) (x x)))
;; #t
;; > (var-occurs-bound? 'z '(lambda (y) (lambda (x) (y z))))
;; #f

(define var-occurs-bound?
  (lambda (a expr)
    (match expr
      [(? symbol?) #f]
     ; [`(lambda (,x) ,e) #:when (not(memv 'lambda e))
      ; ] 
      [`(lambda (,x) ,e)
   ; (cond
        (if (list? e)
             (if (memv 'lambda e) (var-occurs-bound? a e) (if (and (eqv? a x) `(memv ,x ,e)) #t #f)) ;memv a e
             (if (var-occurs? `,a `,expr)
                 (if (and (eqv? a x) `(memv ,x ,e)) #t #f)
                 #f))] 
      
      [e #:when (not(memv 'lambda e)) (if (memv a e) #f #t)]
      [`(,e1 ,e2) `(or ,(var-occurs-bound? e1) ,(var-occurs-bound? e2))]
      ))) 

;; 11. Unique Free vars
;; Examples:
;; > (unique-free-vars '(lambda (x) (x y)))
;; (y)
;; > (unique-free-vars '((lambda (x) ((x y) e)) (lambda (c) (x (lambda (x) (x (e c)))))))
;; (y e x)

(define unique-free-vars
  (lambda (expr)
    (match expr
      [`(lambda (,x) ,e) (remv x (unique-free-vars e))] 
      [(? symbol?) #;when (not (pair? expr))  `(,expr)]       
      [`(,e1 ,e2) (union (unique-free-vars e1) (unique-free-vars e2))])))

;; 12. Unique bound vars
;; Examples:
;; > (unique-bound-vars '(lambda (x) y))
;; ()
;; > (unique-bound-vars '(lambda (x) (x y)))
;; (x)

(define unique-bound-vars
  (lambda (expr)
    (match expr
      [`(lambda (,x) ,e) (if(memv x (unique-vars e)) (cons x (unique-bound-vars e)) (unique-bound-vars e))] 
      [`(,e1 ,e2) (union (unique-bound-vars e1) (unique-bound-vars e2))]
      [e #;when (symbol? e) `()]  ))) 

;; 13. Lex
;; Examples:
;; > (lex '(lambda (y) ((lambda (x) (x y)) (lambda (c) (lambda (d) (y c))))) '()) 
;; (lambda ((lambda ((var 0) (var 1))) (lambda (lambda ((var 2) (var 1))))))

(define lex
  (lambda (expr acc)
    (match expr
      [`,x #:when (symbol? x) `(var ,(- (length acc) (length (memv x acc))))]
      [`(lambda (,x) ,e) `(lambda ,(lex e (cons x acc)))]
      [`(,e1 ,e2) (list `,(lex e1 acc) `,(lex e2 acc))]))) 

;; 14. Walk-symbol-update
;; Examples:
;; > (define a-list `((c . ,(box 15)) (e . ,(box 'f)) (b . ,(box 'c)) (a . ,(box 'b))))
;; > a-list
;; ((c . #&15) (e . #&f) (b . #&c) (a . #&b))
;; > (walk-symbol-update 'a a-list)
;; 15

(define walk-symbol-update
  (lambda (x s)
    (let ([walk_temp (assv x s)]) ;setting up a temp walk with the pair of (x and the corresponding value
      (cond
        [(if (eqv? #f walk_temp)
             x
             (if (pair? walk_temp)
                 (walk-symbol-update (unbox (cdr walk_temp)) s)
                 (unbox (cdr walk_temp)))
             )
             ]))))