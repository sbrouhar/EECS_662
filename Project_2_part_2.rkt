#lang plai


;;Seth J Brouhard
;;EECS 662
;;Project 2: Part 2
;;March 27, 2016

(define-type CFWAE
  (num (n number?))
  (id (name symbol?))
  (add (left CFWAE?) (right CFWAE?))
  (sub (left CFWAE?) (right CFWAE?))
  (div (left CFWAE?) (right CFWAE?))
  (mul (left CFWAE?) (right CFWAE?))
  (fun (param symbol?) (body CFWAE?))
  (app (funct CFWAE?) (arg CFWAE?))
  (if0 (c CFWAE?) (t CFWAE?) (e CFWAE?))
  (with (name symbol?) (named-expr CFWAE?) (body CFWAE?))
  (cond0 (condition CFWAE?) (body CFWAE?)))


(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value CFWAE?) (ds DefrdSub?)])

(define (lookup name ds)
  (type-case DefrdSub ds 
    (mtSub () (error 'lookup "no binding for identifier"))
    (aSub (id value next-ds)
          (if (symbol=? id name)
               value
              (lookup name next-ds)))))

(define prelude
  (aSub 'pi (num 3.141592653589793)
        (aSub 'area (fun 'x (mul (mul (id 'pi)(id 'x))(id 'x)))
              (aSub 'inc (fun 'x (add (id 'x) (num 1))) (mtSub)) 
        )))
        


(define (parse-CFWAE expr)
  (cond
    [(number? expr) (num expr)] 
    [(symbol? expr) (id expr)]
    [(list? expr)
     (case (first expr)
       [(+) (add (parse-CFWAE (second expr)) (parse-CFWAE (third expr)))]
       [(-) (sub (parse-CFWAE (second expr)) (parse-CFWAE (third expr)))]
       [(*) (mul (parse-CFWAE (second expr)) (parse-CFWAE (third expr)))]
       [(/) (div (parse-CFWAE (second expr)) (parse-CFWAE (third expr)))]
       [(if0) (if0 (parse-CFWAE (second expr)) (parse-CFWAE (third expr)) (parse-CFWAE (fourth expr)))]
       [(fun) (fun (second expr) (parse-CFWAE (third expr)))]
       [(with) (app (fun (first (second expr)) (parse-CFWAE (third expr))) (parse-CFWAE (second (second expr))))]
       [(cond0) (cond0 (list (parse-CFWAE (first (second expr))) (parse-CFWAE rest expr)))]
       (else
        [app (parse-CFWAE (first expr)) (parse-CFWAE (second expr))]))]
        ))
 



(define (interp-CFWAE expr ds)
  (type-case CFWAE expr
    [num (n) (num n)]
    [id (x) (lookup x ds)]
    [add (l r) (num (+ (num-n (interp-CFWAE l ds)) (num-n (interp-CFWAE r ds))))]
    [sub (l r) (num (- (num-n (interp-CFWAE l ds)) (num-n (interp-CFWAE r ds))))]
    [mul (l r) (num (* (num-n (interp-CFWAE l ds)) (num-n (interp-CFWAE r ds))))]
    [div (l r) (num (/ (num-n (interp-CFWAE l ds)) (num-n (interp-CFWAE r ds))))]
    (if0 (c t e) 
          (cond ((= (num-n (interp-CFWAE c ds)) 0)
                (interp-CFWAE t ds))
               (else (interp-CFWAE e ds))))
    [fun (para bod) expr]
    [app (func arg)
		(local
              ((define fun-val (interp-CFWAE func ds)))
                  (interp-CFWAE (fun-body fun-val)
				(aSub (fun-param fun-val)
                                  (interp-CFWAE arg ds)
                                  ds )))]
    (with (name named-expr body) 
            (local
              ((define fun-val (interp-CFWAE (fun name body) ds)))
              (interp-CFWAE (fun-body fun-val)
                            (aSub (fun-param fun-val)
                                  (interp-CFWAE named-expr ds)
                                   ds ))))
    (cond0 (con body) (error 'interp "error in interp"))
    ))


(define (elab-CFWAE expr)
    (type-case CFWAE expr
      (num (n) (num n))
      (id (x) (id x))
      (add (l r) (add (elab-CFWAE l) (elab-CFWAE r)))
      (sub (l r) (sub (elab-CFWAE l) (elab-CFWAE r)))
      (mul (l r) (mul (elab-CFWAE l) (elab-CFWAE r)))
      (div (l r) (div (elab-CFWAE l) (elab-CFWAE r)))
      (if0 (c t e) (if0 (elab-CFWAE c) (elab-CFWAE t) (elab-CFWAE e)))
      (fun (p b) (fun p (elab-CFWAE b)))
      (app (funct arg) (app (elab-CFWAE funct) (elab-CFWAE arg)))
      (with (i e b) (app (fun i (elab-CFWAE b)) (elab-CFWAE e)))
      (cond0 (con body) (elab-cond0 con (elab-CFWAE body)))
      ))

       
(define (elab-cond0 con body)
  (cond ((empty? con) body)
        (else (if0 [elab-CFWAE (first (first con))]
                   [elab-CFWAE (second (first con))]
                   [elab-cond0 (second con) body]))))
                   


(define (eval-cfwae expr)
  (interp-CFWAE (elab-CFWAE (parse-CFWAE expr)) prelude))

;(parse-CFWAE '{cond0 {1 2} {0 15} 0})

(test (eval-cfwae '{+ 1 2}) (num 3))
(test (eval-cfwae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfwae '{{fun x x} 3}) (num 3))
(test (eval-cfwae '{{fun x {+ x 1}} 1}) (num 2))
(test (eval-cfwae '{if0 0 1 2}) (num 1))
(test (eval-cfwae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfwae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfwae '{with {x 10} {+ x 5}}) (num 15))
(test (eval-cfwae '{with {f {fun x {+ x 1}}} {f 2}}) (num 3))
(test (eval-cfwae '{cond0 {1 2} {0 15} 0}) (num 15))
(test (eval-cfwae '{with {add1 {fun x {+ x 1}}} {cond0 {{add1 0} 5} {3 4} {0 {add1 2}} 52} 2}) (num 3))
(test (eval-cfwae '{inc pi}) (num 4.141592653589793))
(test (eval-cfwae '{with {x 2} {with {inc {fun x {+ x 2}}} {inc x}}}) (num 4))
(test (eval-cfwae '{area 2}) (num 12.566370614359172))
(test (eval-cfwae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))
(test (eval-cfwae '{with {g {fun f {f 3}}} {g {fun x {+ x 1}}}}) (num 4))
