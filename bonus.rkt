#lang racket
(require racket/match)

;;----------------------- Part 1 SPS ----------------------------------------------------------

;; 1. filter-sps

(define filter-sps
  (lambda (predicate ls store)
    (cond
      [(null? ls) (values '() store)]
      [(predicate (car ls)) (let*-values (((u store) (filter-sps predicate (cdr ls) store)))
                              (values (cons (car ls) u) store))]
      [else (let*-values (((u store) (filter-sps predicate (cdr ls) store)))
              (values u (cons (car ls) store)))])))

;; 2. filter*-sps

(define filter*-sps
  (lambda (f ls store)
    (cond
      [(null? ls) (values '() store)]
      [(pair? (car ls))
       (let*-values (((u store1) (filter*-sps f (car ls) store))
                     ((v store2) (filter*-sps f (cdr ls) store)))
         (values (cons u v) (cons store1 store2)))]
      [(null? (car ls)) (values '() store)]
      [(f (car ls)) (let*-values (((u store) (filter*-sps f (cdr ls) store)))
                              (values (cons (car ls) u) store))]
      [else (let*-values (((u store) (filter*-sps f (cdr ls) store)))
              (values u (cons (car ls) store)))])))

;; 3. fib-sps

(define fib-sps
  (lambda (n store)
    (cond
      ((assv n store) =>
       (lambda (pr) (values (cdr pr) store)))
      ((zero? n) (values n `((,n . 0) . ,store))) ; -> (cons (cons n 0) store)
      ((= 1 n) (values n `((,n . 1) . ,store))) ; -> (cons (cons n 1) store)
      (else
       (let*-values (((u store) (fib-sps (sub1 (sub1 n)) store))
                     ((v store) (fib-sps (sub1 n) store)))
         (values (+ u v) `((,n . ,(+ u v)) . ,store)))))))

;;----------------------- Part 2 Macros ----------------------------------------------------------

;; 4. and*

(define-syntax and*
  (syntax-rules ()
    ((_) #t)
    ((_ op1) op1)
    ((_ op1 op2 ...) (let ((var op1))
                       (if var
                           (and* op2 ...)
                           #f)))))

;; 5. cons*

(define-syntax cons*
  (syntax-rules ()
    ((_) (error 'syntax-error "Incorrect argument-count to cons*"))
    ((_ op1) op1)
    ((_ op1 op2) `(,op1 . ,op2))
    ((_ op1 op2 op3 ...) (cons* op1 (cons* op2 op3 ...)))))

;; 6. macro-list

(define-syntax macro-list
  (syntax-rules ()
    ((_) '())
    ((_ op1) (cons* op1 '()))
    ((_ op1 op2 ...) (cons* op1 (macro-list op2 ...)))))

;; 7. mcond

(define-syntax mcond
  (syntax-rules (else)
    ((_) (void))
    ((_ (else then-body ...)) (begin then-body ...))
    ((_ (e0 op0)) (if e0 op0 (void)))
    ((_ (e0 op0) (e1 op1) ...) (let ((var e0)) 
                                 (if e0
                                     op0
                                     (mcond (e1 op1) ...)))))) 
;; quote-quote
(define-syntax quote-quote
    (syntax-rules ()
      [(_ e) (quote (quote e))]))

#|
-------- My version of copy-code ---------
(define-syntax copy-code
  (syntax-rules ()
    ((_ e) (if (procedure? e) `(,e e) e))))
|#

;; copy-code
(define-syntax copy-code
    (syntax-rules ()
      [(_ x) `(,x x)]))

;; 8. macro-map

(define-syntax macro-map
  (syntax-rules ()
    ((_) (void))
    ((_ proc '()) '())
    ((_ proc `(ls1 ls2 ...)) (list (proc ls1) (proc ls2) ...))))

#;(define-syntax condre
  (syntax-rules ()
    ((_) (void))
    ((_ (else then-body ...)) (begin then-body ...))
    ((_ (let ((e v)) (e0 op0) (e1 op1) ...)) (let ((e v)) (condre (e0 op0) (e1 op1) ...)))
    ((_ (e0 op0)) (if e0 op0 (void)))
    ((_ (e0 op0) (e1 op1) ...) (let ((var e0)) 
                                 (if e0 
                                     op0
                                     (condre (e1 op1) ...)))))) 
