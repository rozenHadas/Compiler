(define not
	(lambda (exp)
		(if exp #f #t)))

(define list (lambda x x))


(define map
	(lambda (proc items)
		(if (null? items)(list)(cons (proc (car items))
			(map proc (cdr items))))))




(define fold-right 
 	(lambda (f init seq) 
   (if (null? seq) 
       init 
       (f (car seq) 
           (fold-right f init (cdr seq)))))) 

(define append
	(lambda s
	 (fold-left binary-append '() s)))

 (define binary-append 
		(lambda (list1 list2)
	(cond ((null? list1) list2)
		  ((and (null? (cdr list1)) (symbol? list2)) (cons (car list1) list2))
          (else (cons (car list1) (binary-append (cdr list1) list2))) )))


(define fold-left 
(lambda (f init seq) 
   (if (null? seq) 
       init 
       (fold-left f 
                  (f init (car seq) ) 
                  (cdr seq))))) 




