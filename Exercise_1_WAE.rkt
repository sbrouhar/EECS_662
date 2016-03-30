#lang plai


;Seth J Brouhard
;KU ID 1349074
;EECS 662
;Project 1 - WAE


;WAE::= <num> | {+ WAE WAE} | {- WAE WAE} | {with {<id> WAE} WAE} | <id>

;Define type WAE that defines what makes up an WAE

(define-type WAE
  (num (n number?))
  (add (lhs WAE?) (rhs WAE?))
  (sub (lhs WAE?) (rhs WAE?))
  (with (name symbol?) (named-exp WAE?) (body WAE?))
  (id (name symbol?)))




(define (subst e i v)
  (type-case WAE e
    (num (n) e)
    (add (l r) (add (subst l i v) (subst r i v)))
    (sub (l r) (sub (subst l i v) (subst r i v)))
    (with (bound-id named-expr bound-body)
          (if (symbol=? bound-id i)
              (with bound-id
                    (subst named-expr i v)
                    bound-body)
              (with bound-id
                    (subst named-expr i v)
                    (subst bound-body i v))))
    (id(val) (if (symbol=? val i) v e))))



(define parse-WAE
  (λ (s-exp)
    (cond
      ((number? s-exp) (num s-exp))
      ((symbol? s-exp) (id s-exp))
      ((list? s-exp)
       (case (first s-exp)
         ((+) (add (parse-WAE (second s-exp)) (parse-WAE (third s-exp))))
         ((-) (sub (parse-WAE (second s-exp)) (parse-WAE (third s-exp))))
         ((with) (with (first (second s-exp)) (parse-WAE (second (second s-exp)))
                       (parse-WAE (third s-exp))))))
       ((error 'parse-WAE "Error in s-exp")))))


(define (calc s-exp)
  (type-case WAE s-exp
    (num(n)n)
    (add(l r)(+(calc l)(calc r)))
    (sub(l r)(-(calc l)(calc r)))
    (with (bound-id named-expr bound-body)
         (calc (subst bound-body
                      bound-id
                      (num(calc named-expr)))))
    (id(v)(error 'calc "free identifier"))))

(define interp-WAE
  (λ (s-exp)
    (calc (parse-WAE s-exp))))

(define eval-WAE
  (λ (s-exp)
   (interp-WAE s-exp)))



(test (eval-WAE (+ 1 3)) 4)
(test (eval-WAE '(with (n 7) (+ n n))) 14)
(test (eval-WAE '(with (n (+ 9 7)) (+ n n))) 32)
(test (eval-WAE '(with (n (+ 9 7)) (+ n n))) 32)
(test (eval-WAE '(with (n (with (x 1) (+ x x))) (+ n n))) 4)
(test (eval-WAE '(with (n (with (x 1) (+ x y))) (+ n z))) 4)
(test (eval-WAE '(with (r (with (s (with (t 2) (+ t t))) (+ s s))) (+ r r))) 16)