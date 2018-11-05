(load "project/qq.scm")


(define <tag_void> 
	(lambda (exp)
		(if (equal? (void) exp)
			`(const ,exp)
			#f
		)
	)
)


(define <tag_const>
	(lambda (exp)
		(if (or (const? exp)(vector? exp) (null? exp))
			(if (null? exp) 
				`(const ,exp)
				(if (quote? exp)
					`(const ,@(cdr exp))
					`(const ,exp))
			)
			#f		
		)
	)
)

(define reserved_words
'(and begin cond define do else if lambda let let* letrec or quasiquote unquote unquote-splicing quote set!))

(define reserved_word? 
	(lambda (w)
	(ormap (lambda (itr) (equal? itr w)) reserved_words)
	)
)

(define <tag_var>
	(lambda (exp) 
		(if (not (list? exp))
			(if (not (reserved_word? exp))
				`(var ,exp)
				#f
			)
			#f
		)
	)
)

(define <tag_cond>
	(lambda (exp)
		(if (list? exp)
			(if (equal? 'if (car exp))
				(if (equal? (length (cdr exp)) 3)
					`(if3 ,(parse (cadr exp)) ,(parse (caddr exp)) ,(parse (cadddr exp)))
					(if (equal? (length (cdr exp)) 2)
						`(if3 ,(parse (cadr exp)) ,(parse (caddr exp)) ,(parse (void)))
					)
				)
				#f)
		#f) 
	)
)


(define <tag_disj>
	(lambda (exp)
		(if (list? exp)
			(if (equal? 'or (car exp))
				(cond ((equal? (length exp) 1) `(const #f))
				      (else 
				      `(or ,(map (lambda (itr) (parse itr)) (cdr exp))))
				)
				#f
			)
		#f)
	)
)

(define member?
  (lambda (item lst)
    (if (or (not (pair? lst)) (null? lst))
		(equal? item lst)
		(if (equal? (car lst) item)
	    	#t
	    	(member? item (cdr lst))
	    )
	)
  )
)

(define valid-params?
  (lambda (lst)
    (if (null? lst) 
	       #t
	   (if (not (pair? lst))
	       (symbol? lst)
	       (if (or  (not (symbol? (car lst))) (member? (car lst) (cdr lst)))
		      #f
		   (valid-params? (cdr lst)))))))

(define reg_lambda
	(lambda (exp)
		(if (valid-params? (car exp))
			`(lambda-simple ,(car exp) ,(parse `(begin ,@(cdr exp))))
			(error 'param "parameters makes no sense!")
		)
	)
)

(define opt_lambda
	(lambda (exp)
		(if (valid-params? (car exp))
			(let ((args (find_args (car exp))))
			`(lambda-opt ,args ,(cdr (last-pair (car exp))) ,(parse `(begin ,@(cdr exp))))
			)
			(error 'param "parameters in opt makes no sense!")
		)
	)
)

(define find_args
	(lambda (lst)
			(if (pair? lst) 
				 (append (list(car lst)) (find_args (cdr lst)))
				'()
				)
		)
)

(define variadic_lambda 
	(lambda (exp)
		(let ((args (car exp))
			   (body (cdr exp)))
				`(lambda-opt () ,args ,(parse `(begin ,@body)))
		)
	)
)

(define <tag_lambda>
	(lambda (exp)
		(if (list? exp)
			(if (equal? 'lambda (car exp))
				(if (list? (cadr exp))
					(reg_lambda (cdr exp))
						(if (pair? (cadr exp))
							(opt_lambda (cdr exp))
							(variadic_lambda (cdr exp))
						)
				)
			#f)
		#f)
	)
)

(define reg_define
	(lambda (exp)
		`(define (var ,(car exp)) ,(parse (cadr exp)))
	)
)

(define Mit_define
	(lambda (exp)
		(let ((tempVar (car exp))
				(parm (cdar exp))
				(body (cadr exp)))
		`(define (var ,(car (find_args tempVar))) ,(parse `(lambda ,parm ,body))))
	)
)

(define <tag_define>
	(lambda (exp)
		(if (list? exp)
			(if (equal? 'define (car exp))
				(if (not (pair? (cadr exp)))
					(reg_define (cdr exp))
					(Mit_define (cdr exp))
				)
			#f)
		#f)
	)
)

(define <tag_assignment>
	(lambda (exp)
		(if (list? exp)
			(if (equal? 'set! (car exp))
				`(set (var ,(cadr exp)) ,(parse (caddr exp)))
				#f
			)
		#f)
	)
)

(define <tag_application>
	(lambda (exp)
		(if (list? exp)
			(if (not (reserved_word?  (car exp)))
				`(applic ,(parse (car exp)) ,(parse-applic (cdr exp)))
				
			#f)

		#f)

	)
)

(define parse-applic
	(lambda(exp)
		(map (lambda(x)(parse x))exp)
	)
)

(define flatSeq 
	(lambda (ls)
	 (fold-left 
	 	(lambda (acc curr)
				(cond 
					((and (list? curr) (equal? (car curr) 'begin)) (append acc (flatSeq (cdr curr))))
					(else (append acc `(,curr))))
			) 
	 '() 
	ls)))

(define <tag_sequence>
	(lambda (exp)
		(if (equal? (car exp) 'begin)
			(cond
				((null? (cdr exp)) `(const ,(void)))
				((equal? (length exp) 2) (parse (cadr exp)))
				(else `(seq ,(map parse (flatSeq (cdr exp))))))
		#f)
	)
)


(define sp_vars
	(lambda (lst)
		(map (lambda (itr) (append (car itr))) lst)
	)
)

(define sp_exps
	(lambda (lst)
		(map (lambda (itr) (append (cadr itr))) lst)
	)
)

(define <macro_let>
	(lambda (exp)
			(if (list? exp)
					(if (equal? 'let (car exp))
						(let ((vars (sp_vars (cadr exp)))
							  (exps (sp_exps (cadr exp))))
						`(applic ,(parse `(lambda ,vars ,@(cddr exp)))  ,(map (lambda (itr) (parse itr)) exps)) 
						)
				#f)
		#f)
	)
)	

(define nested_let
	(lambda (expList body)
			(letrec ((loop
					(lambda (lst b)
						(if (null? (cdr lst))
							  (append `(let  ( ,(car lst))) b)
							`(let  ( ,(car lst) )  ,(loop (cdr lst) b) )
						))))
					(loop expList body)
				)
		)

)

(define <macro_let*>
	(lambda (exp)
			(if (list? exp)
					(if (equal? 'let* (car exp))
						(let ((def_list (cadr exp))
							  (body (cddr exp)))
								(parse (nested_let def_list body))
						)
					#f)
				#f)
	)
)

(define build_letrec
	(lambda (vars exp body)
			`(let ,(map (lambda (lst) (list lst #f)) vars) 
					,@(append `,(map (lambda (lst1 lst2) (list 'set! lst1 lst2)) vars exp)
					body )
			)
	)
)

(define <macro_letrec>
	(lambda (exp)
		(if (list? exp)
					(if (equal? 'letrec (car exp))
						(let ((vars (sp_vars (cadr exp)))
							  (exps (sp_exps (cadr exp))))
							   (parse (build_letrec vars exps (cddr exp)) )
							)
					#f)
			#f)
	)
)

(define nested_and
	(lambda (lst)
		(letrec ((loop
				(lambda (lst)
					(if (null? lst)
						#t
						`(if ,(car lst) ,(loop (cdr lst)) #f)
			))))
	(loop lst))
	)
)

(define <macro_and>
	(lambda (exp)
		(if (list? exp)
			(if (equal? 'and (car exp))
				(cond ((null? (cdr exp)) (parse #t))
					((= (length (cdr exp)) 1) (parse (cadr exp)))
					(else (parse `(if ,(cadr exp) (and ,@(cddr exp)) #f)))) 
			#f)
		#f)
	)
)
(define nested_cond
	(lambda (lst)
		(cond 
			  ((equal? 'else (caar lst)) (parse `(begin ,@(cdar lst))) )
			  ((eq? 1 (length lst)) (parse `(if ,(caar lst) (begin ,@(cdar lst)) )))
			  (else (parse `(if ,(caar lst) (begin ,@(cdar lst))  (cond ,@(cdr lst))) ))
		)

	)
)

(define <macro_cond>
	(lambda (exp)
		(if (list? exp)
			(if (equal? 'cond (car exp))
				 (nested_cond (cdr exp))
			#f)
		#f)
	)
)

(define is_qq? 
	(lambda (lst)
		(equal? 'quasiquote (caar lst)) 
	)
)

(define <quasiquote_expand>
	(lambda (sexp)
		(if (is_qq? (list sexp))
		(parse (expand-qq (cadr sexp)))	 
		#f
		)
	)
)

(define parse
  (lambda (sexp)
  		(or 
  			(<tag_const> sexp)
  			(<tag_void> sexp)
  			(<tag_var> sexp)
  			(<tag_cond> sexp)
  			(<tag_disj> sexp)
  			(<tag_lambda> sexp)
  			(<tag_define> sexp)
  			(<tag_assignment> sexp)
  			(<tag_application> sexp)
  			(<tag_sequence> sexp)
  			(<macro_let> sexp)
  			(<macro_let*> sexp)
  			(<macro_letrec> sexp)
  			(<macro_and> sexp)
  			(<macro_cond> sexp)
  			(<quasiquote_expand> sexp)

  	  	)	
  )
)


