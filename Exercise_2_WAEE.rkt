#lang plai

;Seth J Brouhard
;KU ID 1349074
;EECS 662
;Project 1 - WAEE


;WAEE::= <num> | {+ WAEE WAEE} | {- WAEE WAEE} | {* WAEE WAEE} |  {/ WAEE WAEE} |
;{with {<id> WAEE} WAEE} | <id>

(define-type WAEE
  (num (n number?))
  (id (name symbol?))
  (op (name symbol?))
  (binop (name op?) (lhs WAEE?) (rhs WAEE?))
  (with {binding list?} (body WAEE?)))


(define-type binding
  (binded (name symbol?) (s-exp WAEE?)))


(define-type binop-rec
  (bin-op (name symbol?) (op procedure?)))

(define oper
  (list
   (bin-op 'add +)
   (bin-op 'sub -)
   (bin-op 'mul *)
   (bin-op 'div /)))


(define lookup
  (lambda (op-name op-table)
     (cond ((empty? op-table) (error 'lookup "Operator not found"))
           (else (if (symbol=? (bin-op-name (car op-table)) op-name)
                     (bin-op (car op-table))
                     (lookup op-name (cdr op-table)))))))


(define (subst e i v)
  (type-case WAEE e
    (num (n) e)
    (binop (op l r) (binop op (subst l i v) (subst r i v)))
    (with (bound-binding bound-body)
         (with (map (λ(x) (binded (binded-name x) (subst (binded-s-exp x) i v))))))
         (if (map (λ(x) (if (symbol=? (binded-name) i))
                  binded-s-exp) bound-body (subst bound-body i v))
    (id(val) (if (symbol=? val i) v e)))))



(define parse-WAEE
  (λ (s-exp)
    (cond
      ((number? s-exp) (num s-exp))
      ((symbol? s-exp) (id s-exp))
      ((list? s-exp)
       (case (first s-exp)
         ((+) (binop 'add (parse-WAEE (second s-exp)) (parse-WAEE (third s-exp))))
         ((-) (binop 'sub (parse-WAEE (second s-exp)) (parse-WAEE (third s-exp))))
         ((/) (binop 'div (parse-WAEE (second s-exp)) (parse-WAEE (third s-exp))))
         ((*) (binop 'mul (parse-WAEE (second s-exp)) (parse-WAEE (third s-exp))))
         ((with) (with (map (λ(x) (if (empty? x)
                                      (error )
                                      (binded (first x) (parse-WAEE (second x))))))))
      ((error 'parse-WAEE "Error in s-exp")))))))

(define (calc s-exp)
  (type-case WAEE s-exp
    (num(n) n)
    (binop (op l r) ((lookup op binop-table) (calc l) calc r))
    (with (bound-id named-expr bound-body)
         (calc (subst bound-body
                      bound-id
                      (num(calc named-expr)))))
    (id(v)(error 'calc "free identifier"))))

(define interp-WAEE
  (λ (s-exp)
    (calc (parse-WAEE s-exp))))

(define eval-WAEE
  (λ (s-exp)
   (interp-WAEE s-exp)))


