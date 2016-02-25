#lang racket
(require "numbers.rkt")
(require "mk.rkt")

;; 1. listo
(define listo
  (lambda (ls)
    (conde
     ((nullo ls) succeed)
     ((pairo ls)
      (fresh (d)
             (cdro ls d)
             (listo d))))))
     
;; 2. facto
(define facto
  (lambda (n out)
    (conde
     ((== n '()) (== out '(1))) 
     ((fresh (res sub1res)
             (facto sub1res res)
             (minuso n '(1) sub1res)
             (*o n res out))))))

;; 3. fibso
(define fibso
    (lambda (n out1 out2)
      (conde
        ((== n '()) (== out1 '(1)) (== out2 '(1)))
        ((fresh (n- u v u+v)
               (== out1 v)
               (== out2 u+v)
               (minuso n '(1) n-)
               (fibso n- u v)
               (pluso u v u+v))))))
;; 4. fo-lavo
(define (lookup x vars vals o)
  (fresh (y vars^ a vals^)
    (== `(,y . ,vars^) vars)
    (== `(,a . ,vals^) vals)
    (conde
      ((== x y) (== o a))
      ((=/= x y) (lookup x vars^ vals^ o)))))
  
(define (fo-lavo* exps vars vals o)
  (conde
   ; ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (fo-lavo* exps^ vars vals v^)
         (fo-lavo exp vars vals v)
         )))
    ((== `(tsil) exps) (== o `()))))  
  
(define (fo-lavo exp vars vals o)
  (conde
   ((symbolo exp) (lookup exp vars vals o))
   ((== exp `(,o etouq))
    (absento 'closure o)
    (absento 'etouq vars))
   ((fresh (exps)
           (membero 'tsil exp)
          ;(== exp `(,exps tsil))
           (absento 'tsil vars)
           (fo-lavo* exp vars vals o)))
   ((fresh (x b)
           (== exp `(,b (,x) adbmal))
           (absento 'adbmal vars)
           (symbolo x)
           (== o `(closure ,x ,b ,vars ,vals)))) 
   ((fresh (rator rand)
           (== exp `(,rand ,rator))
           (fresh (x b vars^ vals^ a)
                  (fo-lavo rand vars vals a)
                  (fo-lavo rator vars vals `(closure ,x ,b ,vars^ ,vals^))
                  (fo-lavo b `(,x . ,vars^) `(,a . ,vals^) o))))))

(define (val-ofo* exps vars vals o)
  (conde
    ((== `() exps) (== o `()))
    ((fresh (exp exps^)
       (== exps `(,exp . ,exps^))
       (fresh (v v^)
         (== o `(,v . ,v^))
         (val-ofo exp vars vals v)
         (val-ofo* exps^ vars vals v^))))))

(define (val-ofo exp vars vals o)
  (conde
;;  ((numbero exp) (== o exp))
    ((symbolo exp) (lookup exp vars vals o))
    ((== exp `(quote ,o))
     (absento 'closure o)
     (absento 'quote vars))
    ((fresh (exps)
       (== exp `(list . ,exps))
       (absento 'list vars)
       (val-ofo* exps vars vals o)))
    ((fresh (x b)
       (== exp `(lambda (,x) ,b))
       (absento 'lambda vars)
       (symbolo x)
       (== o `(closure ,x ,b ,vars ,vals))))
    ((fresh (rator rand)
       (== exp `(,rator ,rand))
      (fresh (x b vars^ vals^ a)
        (val-ofo rator vars vals `(closure ,x ,b ,vars^ ,vals^))
        (val-ofo rand vars vals a)
        (val-ofo b `(,x . ,vars^) `(,a . ,vals^) o))))))

;; 5. Color Middle Earth
(define membero
  (lambda (x l)
    (conde
     ((caro l x))
     ((fresh (d)
             (cdro l d)
             (membero x d))))))

(define color-middle-earth
  (lambda (ls)
    (run 1 (q) (colorit ls q))))

(define colorit
  (lambda (ls out)
    (fresh (color1 color2 color3 color4 color5 color6 color7 color8 color9 color10 color11)
           (== `((lindon . ,color1) (forodwaith . ,color2) (eriador . ,color3) (rhovanion . ,color4)
                 (enedwaith . ,color5) (rohan . ,color6) (gondor . ,color7) (rhun . ,color8)
                 (mordor . ,color9) (khand . ,color10) (harad . ,color11)) out)
           
           (=/= color1 color2) 
           (=/= color1 color3)
           
           (=/= color2 color3)
           (=/= color2 color4)
           
           (=/= color3 color4)
           (=/= color3 color5)
           
           (=/= color4 color5)
           (=/= color4 color6)
           (=/= color4 color8)
           
           (=/= color5 color7)
           (=/= color5 color6)
           
           (=/= color6 color7)
           (=/= color6 color8)
           (=/= color6 color10)
           (=/= color6 color11)
           
           (=/= color7 color9)

           (=/= color8 color9)  
           (=/= color8 color10)
           
           (=/= color9 color6)   
           (=/= color9 color10)
           (=/= color9 color11)
           
           (=/= color10 color11)
           
           (membero color1 ls)
           (membero color2 ls)
           (membero color3 ls)
           (membero color4 ls)
           (membero color5 ls)
           (membero color6 ls)
           (membero color7 ls)
           (membero color8 ls)
           (membero color9 ls)
           (membero color10 ls)
           (membero color11 ls))))

;;---------------------- HELPER FUNCTIONS --------------------------
(define nullo
  (lambda (x)
    (== '() x)))

(define conso
  (lambda (a d p)
    (== (cons a d) p)))

(define cdro
  (lambda (p d)
    (fresh (a)
           (== (cons a d) p))))

(define caro
  (lambda (p a)
    (fresh (d)
           (== (cons a d) p))))

(define succeed
  (== #f #f))

(define fail
  (== #t #f))

(define pairo
  (lambda (p)
    (fresh (a d)
           (conso a d p))))

