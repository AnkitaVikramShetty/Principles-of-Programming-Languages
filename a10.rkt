#lang racket
(require "numbers.rkt")
(require "mk.rkt")

;; Part I Write the answers to the following problems using your
;; knowledge of miniKanren.  For each problem, explain how miniKanren
;; arrived at the answer.  You will be graded on the quality of your
;; explanation; a full explanation will require several sentences.

;; 1 What is the value of 

(run 2 (q)
     (== 5 q)
     (conde
      [(conde 
        [(== 5 q)
         (== 6 q)])
       (== 5 q)]
      [(== q 5)]))

;; Answer :
;; The value of the above problem is : '(5)
;; - miniKanren evaluates all goals in the run expression
;;   and returns at most 2 answers.
;; - The first goal (== 5 q) succeeds, associating q with 5
;; - The next goal is a conde expression, which has two lines:
;;     - an inner conde
;;     - (== q 5)
;; - As all goals in a conde line must succeed for the line to
;;   produce one or more values.
;; - Thus, the inner conde returns no values because (== 6 q)
;;   fails as q is already associated with 5 and is no longer fresh.
;; - Since the inner conde is a goal in the first line of the outer conde
;;   and it fails, the first line returns no values.
;; - The second line of the outer conde evaluates the goal (== q 5)
;;   and since 5 is already associated with q, it succeeds.
;; - At the end of the search, q has the value 5 and thus that value
;;   is our output.

;; 2 What is the value of
(run 1 (q) 
     (fresh (a b) 
            (== `(,a ,b) q)
            (absento 'tag q)
            (symbolo a)))
;; Answer :
;; The value of the above problem is : '(((_.0 _.1) (=/= ((_.0 tag))) (sym _.0) (absento (tag _.1))))
;; - run 1 (q) will return at the most 1 value
;; - fresh (a b) creates two fresh variables
;; - (== `(,a ,b) q) associates a list of (_.0 _.1) to q because this goal succeeds  
;; - (absento 'tag q) ensures 'tag doesn't appear in (_.0 _.1) and thus succeeds as (absento (tag _.0) (tag _.1)) 
;; - (symbolo a) associates a to be a symbol as (sym _.0) and thus absento for a can be displayed
;;   using a disequality constraint i.e. (=/= ((_.0 tag))) and (absento (tag _.1)) 
;; - Thus final output is a list of all these values

;; 3 What do the following miniKanren constraints mean?
;; a ==
;; b =/=
;; c absento
;; d numbero
;; e symbolo

;; (a) ==
;;     -> The equality contraint associates the two values it takes with each other i.e unifies two terms
;;        when a variable is fresh. It acts as a constraint when variables are not fresh.

;; (b) =/=
;;     -> The disequality constraint states which two values are not allowed to be associated with each other.
;;        A goal that violates this constraint, fails.

;; (c) absento
;;     -> absento is a constraint that takes two values and puts a constraint that the first value i.e. a symbol tag should be absent
;;        in the second value i.e. a term. If it is present for any goal, it fails.

;; (d) numbero
;;     -> numbero is a constraint that shows that a variable represents a number and on associating it with a non-number
;;        value, the goal fails.

;; (e) symbolo
;;     -> symbolo is a constraint that shows that a variable represents a symbol and on associating it with a non-symbol
;;        value, the goal fails.

;; Part II goes here.
#;(define assoc
  (lambda (x ls)
    (match-let* ((`(,a . ,d) ls)
                 (`(,aa . ,da) a))
      (cond
        ((equal? aa x) a)
        ((not (equal? aa x)) (assoc x d))))))

(define assoco
  (lambda (x ls out)
    (fresh (a d aa da)
           (== `(,a . ,d) ls)
           (== `(,aa . ,da) a)
           (conde
            ((== aa x) (== a out))
            ((=/= aa x) (assoco x d out))))))

#;(define reverse
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else
       (match-let* ((`(,a . ,d) ls)
                    (res (reverse d)))
         (append res `(,a))))))) 
 
(define reverseo
  (lambda (ls out)
    (conde
      ((== '() ls) (== '() out))
      ((fresh (a d res)
       (== `(,a . ,d) ls) 
       (reverseo d res)
       (appendo res `(,a) out)))))) 

#;(define stutter
  (lambda (ls)
    (cond
      ((equal? '() ls) '())
      (else 
        (match-let* ((`(,a . ,d) ls)
		     (res (stutter d)))
          `(,a ,a . ,res))))))
 
(define stuttero
  (lambda (ls out)
    (conde
      ((== '() ls) (== '() out))
      ((fresh (a d res) 
              (== `(,a . ,d) ls)
              (== out `(,a ,a . ,res))
              (stuttero d res))))))

(define lengtho
  (lambda (ls out)
    (conde
     [(== '() ls) (== '() out)] 
     ((fresh (a d res)
             (== `(,a . ,d) ls) 
             (lengtho d res)
             (addero 0 '(1) res out))))))



