#lang plai

;;Seth J Brouhard
;;EECS 662
;;Project 2: Part 1
;;March 27, 2016


(define-type CFAE
  (num (n number?))
  (id (name symbol?))
  (add (left CFAE?) (right CFAE?))
  (sub (left CFAE?) (right CFAE?))
  (div (left CFAE?) (right CFAE?))
  (mul (left CFAE?) (right CFAE?))
  (fun (param symbol?) (body CFAE?))
  (app (funct CFAE?) (arg CFAE?))
  (if0 (c CFAE?) (t CFAE?) (e CFAE?)))


(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value CFAE?) (ds DefrdSub?)])


(define (lookup name ds)
  (type-case DefrdSub ds
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-ds)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-ds))]))




(define (interp-CFAE expr ds)
  (type-case CFAE expr
    [num (n) (num n)]
    [id (x) (lookup x ds)]
    [add (l r) (num (+ (num-n (interp-CFAE l ds)) (num-n (interp-CFAE r ds))))]
    [sub (l r) (num (- (num-n (interp-CFAE l ds)) (num-n (interp-CFAE r ds))))]
    [mul (l r) (num (* (num-n (interp-CFAE l ds)) (num-n (interp-CFAE r ds))))]
    [div (l r) (num (/ (num-n (interp-CFAE l ds)) (num-n (interp-CFAE r ds))))]
    (if0 (c t e) 
          (cond ((= (num-n (interp-CFAE c ds)) 0)
                (interp-CFAE t ds))
               (else (interp-CFAE e ds))))
    [fun (para bod) expr]
    [app (func arg)
		(local
              ((define fun-val (interp-CFAE func ds)))
                  (interp-CFAE (fun-body fun-val)
				(aSub (fun-param fun-val)
                                  (interp-CFAE arg ds)
                                  ds )))]
    ))


(define (parse-CFAE expr)
  (cond
    [(number? expr) (num expr)] 
    [(symbol? expr) (id expr)]
    [(list? expr)
     (case (first expr)
       [(+) (add (parse-CFAE (second expr)) (parse-CFAE (third expr)))]
       [(-) (sub (parse-CFAE (second expr)) (parse-CFAE (third expr)))]
       [(*) (mul (parse-CFAE (second expr)) (parse-CFAE (third expr)))]
       [(/) (div (parse-CFAE (second expr)) (parse-CFAE (third expr)))]
       [(if0) (if0 (parse-CFAE (second expr)) (parse-CFAE (third expr)) (parse-CFAE (fourth expr)))]
       [(fun) (fun (second expr) (parse-CFAE (third expr)))]
       (else
        [app (parse-CFAE (first expr)) (parse-CFAE (second expr))]))]
        ))
    
    
    

    
(define (eval-cfae expr)
  (interp-CFAE (parse-CFAE expr) (mtSub)))


(test (eval-cfae '{+ 1 2}) (num 3))
(test (eval-cfae '{+ 2 {* 2 3}}) (num 8))
(test (eval-cfae '{{fun x x} 3})  (num 3))
(test (eval-cfae '{{fun x {+ x 1}} 1}) (num 2))
(test (eval-cfae '{if0 1 1 2}) (num 2))
(test (eval-cfae '{if0 {{fun x {- x 2}} 3} {{fun x {* 2 x}} 10} {{fun x {/ x 2}} 8}}) (num 4))
(test (eval-cfae '{{if0 0 {fun x {+ x 1}} {fun x {+ x 2}}} 0}) (num 1))
(test (eval-cfae '{{fun x {{fun y {+ x y}} 3}} 1}) (num 4))
