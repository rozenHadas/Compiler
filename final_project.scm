(load "project/sexpr-parser.scm")
(load "project/tag-parser.scm")
(load "project/semantic-analyzer.scm")

(define <void> (if #f #f))

(define file->list
	(lambda (in-file)
		(let ((in-port (open-input-file in-file)))
			(letrec ((run
						(lambda ()
							(let ((ch (read-char in-port)))
								(if (eof-object? ch)
									(begin
									(close-input-port in-port)
									'())
								(cons ch (run)))))))
			(run))))
)

(define pipeline
	(lambda (s)
		((star <sexpr>) s
			(lambda (m r)
				(map (lambda (e)
					(annotate-tc
						 (pe->lex-pe
					 		 (box-set
				 	   			(remove-applic-lambda-nil
									(parse (convert_prog e))
								))))
					)
				m))
		(lambda (f) 'fail))))

 (define convert_prog
	(lambda (prog)
		(letrec ((loop (lambda (exp)
					(if (pair? exp)
						(cond ((ormap (lambda (op) (equal? (car exp) op)) '(+ - * / ))
									(if (> (length (cdr exp)) 2)
										`( ,@(make_binary_calc (car exp) (cdr exp)))
										`(,@exp)))
							  ((ormap (lambda (op) (equal? (car exp) op)) '(< > = ))
									(if (> (length (cdr exp)) 2)
										`( ,@(make_binary_boolean (car exp) (cdr exp)))
										`(,@exp)))
							 ((equal? (car exp) 'apply)
									(if (ormap (lambda (op) (equal? (cadr exp) op)) '(+ - * / ))
										(if (> (length (car (cdaddr exp))) 2)
											`( ,@(make_binary_calc (cadr exp) (car (cdaddr exp))))
											`(,@exp))
										(if (ormap (lambda (op) (equal? (cadr exp) op)) '(< > = ))
											(if (> (length (car (cdaddr exp))) 2)
												`( ,@(make_binary_boolean (cadr exp) (car (cdaddr exp))))
											`(,@exp))
										`(,@exp))))
						 	 ((pair? (car exp))
									`( ,(loop (car exp)) ,@(map loop (cdr exp))))
							(else `( ,(car exp) ,@(map loop (cdr exp)))))
					`( ,@exp))
			)))
		(loop prog)
	)
)) 

(define make_binary_calc 
	(lambda (op lst)
		(let ((param (reverse lst)))
			 (letrec ((loop (lambda (first rest)
				   			(if (equal? (length rest) 1)
				   					`( ,op  ,(car rest) ,first)
				   				`(,op ,(loop (car rest) (cdr rest)) ,first )))))
				(loop (car param) (cdr param)))
		)))

(define make_binary_boolean 
	(lambda (op lst)
		(let* ((paramrev (reverse lst))
				(param1 (list-tail paramrev 1))
				(paramFirst (reverse param1))
				(paramSec (list-tail lst 1)))
				`(and ,@(print_binary_boolean op paramFirst paramSec))
		)))

(define print_binary_boolean
	(lambda (op list1 list2)
		(map (lambda (f s) `( ,op ,f ,s) ) list1 list2 )
	))




(define compile-scheme-file
	(lambda (srcFile trgFile)
		(let* ( (output (open-output-file trgFile 'truncate))
				(afterParser (pipeline (file->list srcFile)))
				(funcLibrary (pipeline (file->list "project/primitive.scm")))
				(parsedFunc (append funcLibrary afterParser))
				(make_list (constract_lists parsedFunc))
				(make_table (constract_tables))
				;(make_table_ass (constract_assembly_table))
				;;(gen (cons-loop code_gen afterParser))	
			)
	;(display "the prog is: \t")
			;(display funcLibrary)
			;(display "\n")
			;(display constLst)
			;(display "\n")
			;(display const_table)
		 (display
	       (string-append 
                "%include \"project/scheme.s\" \n"
                (constract_assembly_table) "\n\n"
	            printSetUp "\n"
	            "\tmov rax, malloc_pointer \n"
				"\tmov qword [rax], start_of_malloc \n"
				(symbol_table_to_ass)
				(gen_library_functions)
               
                ;(cons-loop code_gen parsedFunc) "\n"
  			;(gen-sexprs  (apply string-append (map (lambda (el) (string-append (code-gen el -1) "\n push rax\n" "call write_sob_if_not_void \n"))   parsed-file )))
  				(apply string-append (map (lambda (prog) (string-append (code_gen prog -1) "\n push rax\n" "call write_sob_if_not_void \n"))
  						parsedFunc))
                use_write_sob
                ;"call write_sob_if_not_void \n"
                ;"ret "
                ;epilogue "\n"
			)
		  output) 

	      (close-output-port output)
		)
	)
)

(define printSetUp
	(string-append 
		"section .text\n"
		"main: \n"

		"\tmov rax, 0\n"
		"\tpush rax\n"
		"\tpush rax\n"
		"\tpush Done_Compile\n"
		"\t push rbp\n"
		"\tmov rbp,rsp\n"
	)
) 

(define use_write_sob
	(string-append
		"Done_Compile:\n"
		"\tpop rbx\n"
		"\tmov rax, 0\n"
		"\tcall exit\n"
	)
)

(define current_sub_prog 0)

(define cons-loop
        (lambda (func lst)
                (if (null? lst) 
                    ""
                ;(string-append (func (car lst) -1) (cons-loop func (cdr lst))))))
                    (let ((gen (func (car lst) -1)))
                    	(set! current_sub_prog (+ current_sub_prog 1))
                    (string-append gen (cons-loop func (cdr lst)))))))

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; LIST FOR TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define fvar_library_functions 
    '(cons car cdr map append + apply null? pair? boolean? char? integer? procedure? 
        string? symbol? vector? make-string make-vector not string-length vector-length zero? vector char->integer
        integer->char set-car! set-cdr! string-ref vector-ref symbol->string string->symbol eq? - * / = > < expt
         list remainder denominator numerator number? rational? string-set! vector-set! ))

(define constLst '()) ;`( ,<void> () ,#t ,#f))
(define symbolLst '())
(define globalLst fvar_library_functions)

(define reset_lists
    (lambda ()
        (set! constLst '()) ;`( ,<void> () ,#t ,#f))
        (set! symbolLst '())
        (set! globalLst '()) ;fvar_library_functions)
    )
)


                    

(define constract_lists
	(lambda (prog)
			(letrec ((loop (lambda (prog) 
							(if (pair? prog) 
								(cond ((isIt? (car prog) '(const))
										(build_const_list (cadr prog))) 
									 ((isIt? (car prog) '(var bvar pvar))
									 	(build_const_list (cadr prog)))
									 ((isIt? (car prog) '(fvar define))
									   (build_global_list (cdr prog)))
									((pair? (car prog)) 
										(loop (car prog)) (loop (cdr prog)))
									(else (loop (cdr prog)))
								)
								'()
							))
					))
			(loop prog)
			)
			(remove_duplicate)
			;(TEST_print)
		)
)

(define TEST_print
	(lambda ()
		(display `(CONST LIST: ,constLst))
		(display "\n")
		(display `(GLOBAL LIST: ,globalLst))
		(display "\n")
		(display `(SYMBOL LIST: ,symbolLst))

	)
)

(define make_sub_lists
    (lambda (lst)
        (letrec* (( ans '())
                  (loop (lambda (lst)
                  	(if (not (or (equal? lst (void)) (equal? lst (list)) (equal? lst #t) (equal? lst #f) (boolean? lst)))
                            (cond ((null? lst) lst)
                            	  ((integer? lst) (list lst) )
                				  ((number? lst) (list (numerator lst) (denominator lst) lst))			
                                  ((vector? lst) (build_const_list (list lst)))
                                  ((symbol? lst) (append (list (symbol->string lst)) (list lst)))
                                  ((not (list? lst))
                                   (list lst))
                                  (else (append (loop (car lst)) (loop (cdr lst)) (list lst) ans)))
                       	'() ) )))
            (loop lst))))

(define build_const_list
    (lambda (lst)
        (letrec ((loop (lambda (lst)
        	(if (not (or (equal? lst (void)) (equal? lst (list)) (equal? lst #t) (equal? lst #f)))
            (cond 
                  ((integer? lst) (list lst) )
                  ((number? lst) (list (numerator lst) (denominator lst) lst))
                  ((list? lst) (make_sub_lists lst))
                  ((pair? lst) (list (car lst) (cdr lst) lst))
                  ((vector? lst) 
                        (append (cons_vector_loop make_sub_lists (vector->list lst)) (list lst)))
                  ((symbol? lst) (append (list (symbol->string lst)) (list lst))) ;(build_symbol_list lst))

                  (else (list lst)))

        '()))))
        (set! constLst (append constLst (loop lst)))
        )
	)
)


(define build_symbol_list
	(lambda (s)
		(display s)
		(if (list? s)
		 (if (> (length s) 1)
			(set! symbolLst (append symbolLst (list (car s)))) 
			(set! symbolLst (append symbolLst s))
		 ) 
		 (if (not (null? s))
			(set! symbolLst (append symbolLst (list s))))
		)
	)
)

(define build_global_list
	(lambda (g)
		(if (> (length g) 1)
			(let ((item (cdar g)))
				(set! globalLst (append globalLst item))
				(constract_lists (cdr g)))
			(set! globalLst (append globalLst g))
		)
	)
)


(define remove_duplicate
	(lambda () 
		(set! constLst (remove_dup constLst))
		(set! symbolLst (remove_dup symbolLst))
		(set! globalLst (remove_dup globalLst))
	)
)

(define remove_dup 
    (lambda (lst)
    	(if (not (null? lst))
	        (fold-left (lambda (acc el)
	                        (if (member el acc)
	                            acc
	                            (append acc (list el))))
	                    '()
	                    lst)
	    '())))

(define cons_vector_loop
        (lambda (func lst)
                (if (null? lst) 
                    '()
                    (let ((res (func (car lst))))
                    (append res (cons_vector_loop func (cdr lst)))))))

(define isIt? 
	(lambda (e formList)
			(ormap (lambda (form) (equal? e form)) formList)
	)
)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define START_MEMORY 20)
(define memory_counter START_MEMORY)
(define start_constT_memo 20)
(define start_symbolT_memo 20)
(define start_globalT_memo 20)
(define const_table '())
(define global_table '())
(define symbol_table '())

(define inc_get_memory_counter
    (lambda (n)
        (let ((old memory_counter))
            (set! memory_counter (+ memory_counter n))
                old)))

(define constract_tables
	(lambda ()
		(build_const_table)
		;(set! start_symbolT_memo memory_counter)
		;(build_symbol_table)
		(set! start_globalT_memo memory_counter)
		(build_global_table)
		;(TEST_print_tables)
	)
)

(define TEST_print_tables
	(lambda ()
		(display "
			CONST TABLE: 
		")
		(display const_table)
		;(display symbol_table)
		(display "
			GLOBAL TABLE: 
		")
		(display global_table)
	)
)

(define build_const_table
	(lambda ()
		;(let* cList constLst)
		(set! const_table 
			(list 
				 `( ,(inc_get_memory_counter 1) ,(void) ("sobVoid") )
				 `( ,(inc_get_memory_counter 1) ,(list) ("sobNil"))
				 `( ,(inc_get_memory_counter 2) ,#f ("sobFalse"))
				 `( ,(inc_get_memory_counter 2) ,#t ("sobTrue"))
				)
             )
		(cMap (lambda (item) (set! const_table (append const_table (list (create_row_const_table item))))) constLst);cList)
	)

)

(define count_label_const_table 0)
(define set_count 
	(lambda (num)
		(let ((old count_label_const_table))
 	(set! count_label_const_table (+ count_label_const_table num))
	 old)))


(define create_row_const_table
	(lambda (item)
		(cond
			((integer? item) 
				(let ((label_int (if (< item 0) (string-append "sobInt_neg" (number->string (* -1 item)))
												 (string-append "sobInt_" (number->string item)))))
									`( ,(inc_get_memory_counter 2) ,item (,label_int ,item))))
			((number? item) (let ((label_frac (if (< item 0) (string-append "sobFrac_neg" (number->string (* -1 (numerator item))) "div"(number->string (denominator item)))
															 (string-append "sobFrac_" (number->string (numerator item)) "div"(number->string (denominator item))))))
							`( ,(inc_get_memory_counter 3) ,item (,label_frac ,(number->string (numerator item)) ,(number->string (denominator item)) ))))
			((char? item) (let ((num_label (set_count 1))
								(label_char (string-append "sobChar_"(number->string count_label_const_table))))
							`(,(inc_get_memory_counter 2) ,item (,label_char ,(char->integer item)))))
			((string? item) (let ((strLen (string-length item))
								 (num_label (set_count 1)) ;update num of label
								(label_string (string-append "sobString_" (number->string count_label_const_table)))
								(get_chars (map (lambda (el) (char->integer el)) (string->list item))))
								`( ,(inc_get_memory_counter (+ 2 strLen)) ,item (,label_string ,strLen ,@get_chars))))
			((symbol? item) (let* ((num_label (set_count 1))
									(label_symbol (string-append "sobSymbol_"(number->string count_label_const_table)))
									(temp 	(build_symbol_table label_symbol)))
							`( ,(inc_get_memory_counter 2) ,item (,label_symbol ,item )))) 
			((pair? item) (let ((num_label (set_count 1))
								(label_pair (string-append "sobPair_" (number->string count_label_const_table))))
							`( ,(inc_get_memory_counter 3) ,item (,label_pair ,(get_label (car item) const_table) ,(get_label (cdr item) const_table)))))
			((vector? item)  (let ((num_label (set_count 1))
									(addresses (map (lambda (el) (get_label el const_table)) (vector->list item)))
									(label_vector (string-append "sobVector_" (number->string count_label_const_table))))
                              `(,(inc_get_memory_counter (+ (vector-length item) 2)) ,item (,label_vector ,(vector-length item) ,@addresses))))

			(else 'ERROR_CONST_TABLE )
		)
	)
)

(define build_symbol_table
	(lambda (symLabel)
		(set! symbol_table (append symbol_table (list symLabel)))
	)
)

(define build_global_table
	(lambda ()
		(cMap (lambda (item) (set! global_table (append global_table (list (create_row_global_table item))))) globalLst)
	)
)

(define create_row_global_table
	(lambda (item)
			 `(,item ,(string-append "Lglobal_" (number->string (inc_get_memory_counter 1))))		
	)
)



(define cMap
    (lambda (proc lst)
        (fold-left
            (lambda(acc el)
                (append acc (list (proc el))))
            '()
            lst)))

(define get_memo_loc
	(lambda (item table)
        (caar (filter (lambda (el) (equal? item (cadr el))) table))
    )
)

(define get_label
	(lambda (item table)
        (car (caddar (filter (lambda (el) (equal? item (cadr el))) table)))
    )
)

(define get_fvar_label
	(lambda (item)
        (cadar (filter (lambda (el) (equal? item (car el))) global_table))
    )
)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TABLE TO ASSEMBLY ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define table_ass_list_string '())

(define constract_assembly_table
	(lambda ()
		(let ((constAss (const_table_to_ass))
				(symbolAss (defult_symbol_table_to_ass))
				(globalAss (global_table_to_ass)))
			;(set! table_ass_list_string (string-append constAss globAss))
			 ;table_ass_list_string	
			 (string-append constAss "\n" symbolAss "\n" globalAss "\n" )
			)	
	)
)

(define gen_library_functions
	(lambda ()
		(string-append
			""
			(lib_Car) (lib_Cdr) (lib_Is_Boolean) (lib_Is_Null) (lib_Is_Char) (lib_Is_Integer) (lib_Is_Pair) (lib_Is_Procedure) 
			(lib_Is_String) (lib_Is_Vector) (lib_Is_Symbol) (lib_Is_Zero) (lib_Is_Number) (lib_Is_Rational) (lib_Is_Eq) (lib_Vector)
			(lib_Make_string)(lib_Make_vector) (lib_binary_plus) (lib_binary_sub) (lib_cons) (lib_binary_div) (lib_binary_mul) (lib_binary_equal)
			(lib_numerator) (lib_denominator) (lib_remainder) (lib_char_to_integer) (lib_integer_to_char) (lib_symbol_to_string)  (lib_string_length) (lib_string_ref)
			(lib_string_set) (lib_vector_length) (lib_vector_ref) (lib_vector_set) (lib_make_apply) (lib_grater) (lib_smaller) (lib_string_to_symbol)
			
		)))

(define lib_Car
	(lambda ()
		(let ((label (get_fvar_label 'car )))
			
			(string-append
			
			"\tjmp Car_Body\n"
			"L_Car:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov rax, An(0)\n"
			"\tCAR rax\n"  
			"\tpop rbp\n"
			"\tret\n\n"

			"Car_Body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , L_Car\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

			))))

(define lib_Cdr
	(lambda ()
		(let ((label (get_fvar_label 'cdr)))
			(string-append

			"\tjmp Cdr_Body\n"
			"L_Cdr:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov rax, An(0)\n"
			"\tCDR rax\n"  
			"\tpop rbp\n"
			"\tret\n\n"

			"Cdr_Body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , L_Cdr\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))


(define lib_Is_Boolean
	(lambda ()
		(let ((label (get_fvar_label 'boolean?)))
			(string-append
				
			"\tjmp Boolean_body\n"
			"Is_Boolean:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_BOOL\n"
			"\tje Bool_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Bool_EXIT\n"
			"Bool_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Bool_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Boolean_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Boolean\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"			

			))))

(define lib_Is_Null
	(lambda ()
		(let ((label (get_fvar_label 'null?)))
			(string-append
			"\tjmp Null_body\n"
			"Is_Null:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_NIL\n"
			"\tje Null_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Null_EXIT\n"
			"Null_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Null_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Null_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Null\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"	

			))))

(define lib_Is_Char
	(lambda ()
		(let ((label (get_fvar_label 'char?)))	
			(string-append

			"\tjmp Char_body\n"
			"Is_Char:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_CHAR\n"
			"\tje Char_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Char_EXIT\n"
			"Char_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Char_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Char_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Char\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"	

			))))

(define lib_Is_Integer
	(lambda ()
		(let ((label (get_fvar_label 'integer?)))
			(string-append

			"\tjmp Integer_body\n"
			"Is_Integer:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_INTEGER\n"
			"\tje Integer_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Integer_EXIT\n"
			"Integer_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Integer_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Integer_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Integer\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Is_Pair
	(lambda ()
		(let ((label (get_fvar_label 'pair?)))
			(string-append
			"\tjmp Pair_body\n"
			"Is_Pair:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_PAIR\n"
			"\tje Pair_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Pair_EXIT\n"
			"Pair_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Pair_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Pair_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Pair\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Is_Procedure
	(lambda ()
		(let ((label (get_fvar_label 'procedure?)))
			(string-append
			"\tjmp Procedure_body\n"
			"Is_Procedure:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_CLOSURE\n"
			"\tje Procedure_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Procedure_EXIT\n"
			"Procedure_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Procedure_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Procedure_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Procedure\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Is_String
	(lambda ()
		(let ((label (get_fvar_label 'string?)))
			(string-append

			"\tjmp String_body\n"
			"Is_String:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_STRING\n"
			"\tje String_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp String_EXIT\n"
			"String_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"String_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"String_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_String\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

			))))

(define lib_Is_Vector
	(lambda ()
		(let ((label (get_fvar_label 'vector?)))
			(string-append

			"\tjmp Vector_body\n"
			"Is_Vector:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_VECTOR\n"
			"\tje Vector_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Vector_EXIT\n"
			"Vector_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Vector_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Vector_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Vector\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Is_Symbol
	(lambda ()
		(let ((label (get_fvar_label 'symbol?)))
			(string-append

			"\tjmp Symbol_body\n"
			"Is_Symbol:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_SYMBOL\n"
			"\tje Symbol_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Symbol_EXIT\n"
			"Symbol_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Symbol_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Symbol_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Symbol\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Is_Zero
	(lambda ()
		(let ((label (get_fvar_label 'zero?)))
			(string-append

			"\tjmp Zero_body\n"
			"Is_Zero:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tDATA r8\n" 
			"\tcmp  r8, 0\n"
			"\tje Zero_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Zero_EXIT\n"
			"Zero_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Zero_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Zero_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Zero\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Is_Number
	(lambda ()
		(let ((label (get_fvar_label 'number?)))

			(string-append
			
			"\tjmp Number_body\n"
			"Is_Number:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_INTEGER\n"
			"\tje Integer_Num_True\n"
			"\tcmp r8, T_FRACTION\n"
			"\tje Fraction_Num_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Number_EXIT\n"
			"Integer_Num_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Fraction_Num_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Number_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Number_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Number\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Is_Rational
	(lambda ()
		(let ((label (get_fvar_label 'rational?)))

			(string-append
			
			"\tjmp Rational_body\n"
			"Is_Rational:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tand r8, 15\n" 
			"\tcmp  r8, T_INTEGER\n"
			"\tje Ratio_True\n"
			"\tcmp r8, T_FRACTION\n"
			"\tje Ratio_True\n"
			"\tmov rax , SOB_FALSE\n"
			"\tjmp Rational_EXIT\n"
			"Ratio_True:\n"
			"\tmov rax, SOB_TRUE\n"
			"Rational_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"Rational_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Rational\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))
(define lib_Is_Eq
	(lambda ()
		(let ((label (get_fvar_label 'eq?)))
			(string-append
			
			"\tjmp Equal_body\n"
			"Is_Equal:\n" 
		 	"\tpush rbp\n"
			"\tmov rbp,rsp\n"
			"\tmov r9,An(0)\n"
			"\tmov r10,An(1)\n"
			"\tcmp r9, r10\n"
			"\tje Eq_True\n"

			"Eq_False:\n"
			"\tmov rax, SOB_FALSE\n"
			"\tjmp Eq_EXIT\n"

			"Eq_True:\n"
			"\tmov rax, SOB_TRUE\n"

			"Eq_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"
			
			"Equal_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Is_Equal\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Make_string
	(lambda ()
		(let ((label (get_fvar_label 'make-string))
				(char_def (number->string 0)))
			(string-append

			"\tjmp Make_String_body\n"
			
			"\tMake_String:\n"	
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"

			"\tmov r15,"char_def"\n"
			"\tmov r10, An(0)\n"; num of iteration
			"\tDATA r10\n"
			"\tmov r8, arg_count\n"
			"\tcmp r8, 1\n"
			"\tje make_string_impl\n"
			"\tmov r15, An(1)\n";char to convert
			
			"make_string_impl:\n"
			"\tcmp r10, 0\n"
			"\tje String_zero\n"
			"\tDATA r15\n"
			"\tmov r12 , 0\n"
			"\tMY_MALLOC(r10)\n"
			
			"make_string_impl_loop:\n"
			"\tcmp r12, r10\n"
			"\tje make_string_loop_END\n"
			"\tadd [rax+r12], r15\n"; r12 = answer
			"\tinc r12\n"
			"\tjmp make_string_impl_loop\n"

			"String_zero:\n"
			"\tmov rax,0\n"
			"\tor rax, T_STRING\n"
			"\tjmp String_end\n"
			
			"make_string_loop_END:\n"
			"\tMY_MAKE_STRING r12, rax\n"
			"\tmov rax, r12\n"
			"\tpush rax \n"
			
			"String_end:\n"
			"\tleave\n"
			"\tret\n"
			
			"Make_String_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, -1 , Make_String\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_Make_vector
	(lambda ()
		(let ((label (get_fvar_label 'make-vector))
			  (char_def (number->string 0)))
			(string-append
			"\tjmp make_vector_body\n"
			
			"make_vector:\n"	
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"

			"\tmov r9, A0\n"
			"\tDATA r9\n"
			"\tmov r11, 03\n"  ;defult char
			"\tmov r8, arg_count\n" ;num of argument 
			"\tcmp r8, 1\n"
			"\tje make_vector_impl\n"
			"\tmov r11, An(1)\n" ;real char
			
			"make_vector_impl:\n"
			"\tmov r15, 0\n" ;index
			"\tDATA r15\n"
			"\tcmp r9, 0\n"
			"\tje make_vector_zero\n"
			"\tmov r12, A0\n"
			"\tmov r14, r12\n"
			"\tshl r14, 3\n"
			"\tMY_MALLOC(r14)\n"
			"\tmov rdi, rax\n" ;ans in rbx 

			"make_vector_impl_loop:\n"
			"\tcmp r15, r9\n"
			"\tje make_vector_loop_END\n"
			"\tMY_MALLOC(8)\n"
			"\t mov [rax], r11\n"
			"\tmov [rdi+r15*8] ,rax\n"
			"\tinc r15\n"
			"\tjmp make_vector_impl_loop\n"

			"make_vector_zero:\n"
			"\tMY_MALLOC(8)\n"
			"\tmov rax,0\n"
			"\tor rax, T_VECTOR\n"
			"\tjmp make_vector_end\n"
			
			"make_vector_loop_END:\n"
			"\tMY_MALLOC(16)\n"
			"\tshl r9, 30\n"
			"\tsub rdi, start_of_data\n"
			"\tor r9, rdi\n"
			"\tshl r9, TYPE_BITS\n"
			"\tor r9, T_VECTOR\n"

			"\tmov [rax], r9\n"
			"\tmov rax,[rax]\n"
			
			"make_vector_end:\n"
			"\tleave\n"
			"\tret\n"
			
			"make_vector_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, 0 , make_vector\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			)
		)
	)
)

(define lib_Vector
	(lambda ()
		(let ((label (get_fvar_label 'vector)))
			(string-append

			"\tjmp vector_body\n"
			
			"\tvector:\n"	
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"

			"\tmov r8, arg_count\n" ;num of argument 
			"\tcmp r8, 0\n"
			"\tje Vector_zero\n"
			"\tmov r15, 0\n" ;index
			"\tmov r14, r8\n"
			"\tshl r14, 3\n"
			"\tMY_MALLOC(r14)\n"
			"\tmov rdi, rax\n" ;ans in rbx 


			"vector_impl_loop:\n"
			"\tcmp r15, r8\n"
			"\tje vector_loop_END\n"
			"\tmov r10, r15\n"
			"\tmov r11, An(r10)\n"
			"\tMY_MALLOC(16)\n"
			"\t mov [rax], r11\n"

			"\tmov [rdi+r15*8] ,rax\n"
			"\tinc r15\n"
			"\tjmp vector_impl_loop\n"

			"Vector_zero:\n"
			"\tMY_MALLOC(16)\n"
			"\tmov rax,0\n"
			"\tor rax, T_VECTOR\n"
			"\tjmp Vector_end\n"
			
			"vector_loop_END:\n"
			"\tMY_MALLOC(16)\n"
			"\tshl r8, 30\n"
			"\tsub rdi, start_of_data\n"
			"\tor r8, rdi\n"
			"\tshl r8, TYPE_BITS\n"
			"\tor r8, T_VECTOR\n"

			"\tmov [rax], r8\n"
			"\tmov rax,[rax]\n"
			
			"Vector_end:\n"
			"\tleave\n"
			"\tret\n"
			
			"vector_body:\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, 0 , vector\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			)
		)
	)
)

(define lib_binary_plus
	(lambda ()
		(let ((label (get_fvar_label '+)))
		(string-append

		"\tjmp Binary_add_body\n"

		"Binary_add:\n"
		"\tpush rbp\n"
		"\tmov rbp,rsp\n"
		"\tmov r8, arg_count\n"
		"\tcmp r8, 0\n"
		"\tje Binary_add_NO_PARAMETERS\n"
		"\tcmp r8,1 \n"
		"\tjne Binary_add_REGULAR \n"

		"Binary_add_ONE_PARAMETER:\n"
		"\tmov r8, An(0)\n"
		"\tmov rax, r8\n"
		"\tjmp Binary_add_END\n"


		"Binary_add_REGULAR:\n"
		"\tmov r8, An(0)\n"
		"\tmov r9, An(1)\n"

		"AFTER_CONDITIONS_OF_PARAMETERS:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9 \n"; transfer the args to temp register
		"\tand r10, 15\n" ; put type of r8 in r10
		"\tand r11, 15\n" ; put type of r9 in r11

		"\tcmp r10, T_INTEGER\n" 		;check if first arg is intger 
		"\tje CONVERT_ARG1_TO_FRACTION\n"
			 
		"\tjmp AFTER_ARG1_CHECKED\n"


		"CONVERT_ARG1_TO_FRACTION:\n"
		"\tmov rdi,r8\n"   			
		"\tDATA_LOWER rdi\n"
	  	"\tshl rdi,30\n"		 ;mone
		"\tmov rcx,1\n"  		;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r8,rdi\n"

		"AFTER_ARG1_CHECKED:\n"
		"\tcmp r11, T_INTEGER\n" ; check intger of first arg
		"\tje CONVERT_ARG2_TO_FRACTION\n"
			 
		"\tjmp AFTER_ARG2_CHECKED\n"

		"CONVERT_ARG2_TO_FRACTION:\n"
		"\tmov rdi,r9\n"   
		"\tDATA_LOWER rdi\n"
		"\tshl rdi,30\n" 				;mone
		"\tmov rcx,1\n"  				;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r9,rdi\n"
		   
		"AFTER_ARG2_CHECKED:\n" 

		"PLUS_TWO_ARGS_FRACTION:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9\n"
		"\tDATA_UPPER_MEIR r10\n" ; r10 = mone  arg 1 
		"\tDATA_UPPER_MEIR r11\n"; r11 = mone  arg 2 
		"\tmov r12, r8\n"
		"\tmov r13, r9\n"
		"\tDATA_LOWER_MEIR r12 \n";  r12 = mehane  arg 1
		"\tDATA_LOWER_MEIR r13  \n"; r12 = mehane  arg 2

		"\tmov rax, r10\n"
		"\tcqo\n"
		"\timul r13 \n" 	
		"\tmov r15, rax \n"   ; (mehane arg 2 * mone arg 1) = r15 

		"\tmov rax, r11\n"
		"\tcqo\n"
		"\timul r12 \n"
		"\tmov r14, rax \n"   ; (mehane arg 1 * mone arg 2 ) = r14

		"\tadd r15, r14\n"  ; now r15 is the result of adding multiplactions- (the mone of the new fraction) 

		"\tmov rax, r12\n"
		"\tcqo\n"
		"\timul r13 \n"   
		"\tmov r8, rax \n"  ; (mehane arg1 * mehane arg 2)   = r8 -this is the mehane of the new fraction; [reuse of r8 beacause i dont need the orginal value anymore]

		"\tpush r8  \n"  ;  mehane of new fraction
		"\tpush r15\n"  ;   mone of new fraction
		"\tcall gcd\n"

		"\tmov r9, rax\n" ;  gcd result in r9 [reuse of r9  beacause i dont need the orginal value anymore]
		"\tpop r15\n"
		"\tpop r8\n"
		"\tmov rax, r15\n"
		"\tcqo\n"
		"\tidiv r9  \n"  
		"\tmov r15, rax \n" ; r15 = (mona / gcd)

		"\tmov rax, r8\n"
		"\tcqo\n"
		"\tidiv r9\n"
		"\tmov r8, rax  \n" ; r8 = (maehane / gcd)
		"\tshl r15,30 \n";   r15 is mone 
		"\tor r15, r8  \n"; with "or" turn r8 to mehane 

		"\tshl r15, TYPE_BITS \n"; change the type   
		"\tor r15, T_FRACTION\n"
		

		"CHECK_REDUCE_TO_INTEGER:\n"
		"\tmov r8, r15\n"

		"\tDATA_LOWER_MEIR r8 \n" ;mehane
		"\tmov r9,r15\n"
		"\tDATA_UPPER_MEIR r9  \n"  ;mone

		"\tmov rax,r9\n"
		"\tmov rdx,0\n"
		"\tcqo\n"
		"\tidiv r8\n"

		"\tcmp rdx,0    \n" 	; rdx = remainder
		"\tje Binary_add_CHANGE_TO_INTEGER\n"
		"\tmov rax, r15\n"
		"\tjmp Binary_add_END\n"

		"Binary_add_NO_PARAMETERS:\n"
		"\tmov rax,0\n"

		"Binary_add_CHANGE_TO_INTEGER:\n"
		"\tshl rax, TYPE_BITS\n"
		"\tor rax, T_INTEGER\n"

		"\tjmp Binary_add_END\n"

		"Binary_add_END:	\n"
		"\tpop rbp\n"
		"\tret	\n"

		"Binary_add_body:\n"
		"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , Binary_add\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

	))))

(define lib_binary_sub
	(lambda ()
		(let ((label (get_fvar_label '-)))
		(string-append

		"\tjmp Binary_sub_body\n"

		"Binary_sub:\n"
		"\tpush rbp\n"
		"\tmov rbp,rsp\n"

		"\tmov r8, arg_count\n"
		"\tcmp r8,1 \n"
		"\tjne Binary_sub_REGULAR \n"

		"Binary_sub_ONE_PARAMETER:\n"
		"\txor rax, rax\n"
		"\tmov r9, An(0)\n"
		"\tDATA r9\n"
		"\tmov r10, r9\n"
		"\tsub rax, r10\n"
		"\tjmp Binary_sub_CHANGE_TO_INTEGER\n"



		"Binary_sub_REGULAR:\n"
		"\tmov r8, An(0)\n"
		"\tmov r9, An(1)\n"

		"AFTER_CONDITIONS_OF_PARAMETERS_SUB:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9 \n"; transfer the args to temp register
		"\tand r10, 15\n" ; put type of r8 in r10
		"\tand r11, 15\n" ; put type of r9 in r11

		"\tcmp r10, T_INTEGER\n" 		;check if first arg is intger 
		"\tje CONVERT_ARG1_TO_FRACTION_SUB\n"
			 
		"\tjmp AFTER_ARG1_CHECKED_SUB\n"


		"CONVERT_ARG1_TO_FRACTION_SUB:\n"
		"\tmov rdi,r8\n"   			
		"\tDATA_LOWER rdi\n"
	  	"\tshl rdi,30\n"		 ;mone
		"\tmov rcx,1\n"  		;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r8,rdi\n"

		"AFTER_ARG1_CHECKED_SUB:\n"
		"\tcmp r11, T_INTEGER\n" ; check intger of first arg
		"\tje CONVERT_ARG2_TO_FRACTION_SUB\n"
			 
		"\tjmp AFTER_ARG2_CHECKED_SUB\n"

		"CONVERT_ARG2_TO_FRACTION_SUB:\n"
		"\tmov rdi,r9\n"   
		"\tDATA_LOWER rdi\n"
		"\tshl rdi,30\n" 				;mone
		"\tmov rcx,1\n"  				;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r9,rdi\n"
		   
		"AFTER_ARG2_CHECKED_SUB:\n" 

		"SUB_TWO_ARGS_FRACTION:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9\n"
		"\tDATA_UPPER_MEIR r10\n" ; r10 = mone  arg 1 
		"\tDATA_UPPER_MEIR r11\n"; r11 = mone  arg 2 
		"\tmov r12, r8\n"
		"\tmov r13, r9\n"
		"\tDATA_LOWER_MEIR r12 \n";  r12 = mehane  arg 1
		"\tDATA_LOWER_MEIR r13  \n"; r12 = mehane  arg 2

		"\tmov rax, r10\n"
		"\tcqo\n"
		"\timul r13 \n" 	
		"\tmov r15, rax \n"   ; (mehane arg 2 * mone arg 1) = r15 

		"\tmov rax, r11\n"
		"\tcqo\n"
		"\timul r12 \n"
		"\tmov r14, rax \n"   ; (mehane arg 1 * mone arg 2 ) = r14

		"\tsub r15, r14\n"  ; now r15 is the result of sub multiplactions- (the mone of the new fraction) 

		"\tmov rax, r12\n"
		"\tcqo\n"
		"\timul r13 \n"   
		"\tmov r8, rax \n"  ; (mehane arg1 * mehane arg 2)   = r8 -this is the mehane of the new fraction; [reuse of r8 beacause i dont need the orginal value anymore]

		"\tpush r8  \n"  ;  mehane of new fraction
		"\tpush r15\n"  ;   mone of new fraction
		"\tcall gcd\n"

		"\tmov r9, rax\n" ;  gcd result in r9 [reuse of r9  beacause i dont need the orginal value anymore]
		"\tpop r15\n"
		"\tpop r8\n"
		"\tmov rax, r15\n"
		"\tcqo\n"
		"\tidiv r9  \n"  
		"\tmov r15, rax \n" ; r15 = (mona / gcd)

		"\tmov rax, r8\n"
		"\tcqo\n"
		"\tidiv r9\n"
		"\tmov r8, rax  \n" ; r8 = (maehane / gcd)
		"\tshl r15,30 \n";   r15 is mone 
		"\tor r15, r8  \n"; with "or" turn r8 to mehane 

		"\tshl r15, TYPE_BITS \n"; change the type   
		"\tor r15, T_FRACTION\n"
		

		"CHECK_REDUCE_TO_INTEGER_SUB:\n"
		"\tmov r8, r15\n"

		"\tDATA_LOWER_MEIR r8 \n" ;mehane
		"\tmov r9,r15\n"
		"\tDATA_UPPER_MEIR r9  \n"  ;mone

		"\tmov rax,r9\n"
		"\tmov rdx,0\n"
		"\tcqo\n"
		"\tidiv r8\n"

		"\tcmp rdx,0    \n" 	; rdx have remainder?
		"\tje Binary_sub_CHANGE_TO_INTEGER\n"
		"\tmov rax, r15\n"
		"\tjmp Binary_sub_END\n"


		"Binary_sub_CHANGE_TO_INTEGER:\n"
		"\tshl rax, TYPE_BITS\n"
		"\tor rax, T_INTEGER\n"

		"Binary_sub_END:	\n"
		"\tpop rbp\n"
		"\tret	\n"

		"Binary_sub_body:\n"
		"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , Binary_sub\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

	))))

(define lib_binary_mul
	(lambda ()
		(let ((label (get_fvar_label '*)))
		(string-append

		"\tjmp Binary_mul_body\n"

		"Binary_mul:\n"
		"\tpush rbp\n"
		"\tmov rbp,rsp\n"
		"\tmov r8, arg_count\n"
		"\tcmp r8, 0\n"
		"\tje Binary_mul_NO_PARAMETERS\n"
		"\tcmp r8,1 \n"
		"\tjne Binary_mul_REGULAR \n"

		"Binary_mul_ONE_PARAMETER:\n"
		"\tmov r8, An(0)\n"
		"\tmov rax, r8\n"
		"\tjmp Binary_mul_END\n"


		"Binary_mul_REGULAR:\n"
		"\tmov r8, An(0)\n"
		"\tmov r9, An(1)\n"

		"AFTER_CONDITIONS_OF_PARAMETERS_MUL:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9 \n"; transfer the args to temp register
		"\tand r10, 15\n" ; put type of r8 in r10
		"\tand r11, 15\n" ; put type of r9 in r11

		"\tcmp r10, T_INTEGER\n" 		;check if first arg is intger 
		"\tje CONVERT_ARG1_TO_FRACTION_MUL\n"
			 
		"\tjmp AFTER_ARG1_CHECKED_MUL\n"


		"CONVERT_ARG1_TO_FRACTION_MUL:\n"
		"\tmov rdi,r8\n"   			
		"\tDATA_LOWER rdi\n"
	  	"\tshl rdi,30\n"		 ;mone
		"\tmov rcx,1\n"  		;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r8,rdi\n"

		"AFTER_ARG1_CHECKED_MUL:\n"
		"\tcmp r11, T_INTEGER\n" ; check intger of first arg
		"\tje CONVERT_ARG2_TO_FRACTION_MUL\n"
			 
		"\tjmp AFTER_ARG2_CHECKED_MUL\n"

		"CONVERT_ARG2_TO_FRACTION_MUL:\n"
		"\tmov rdi,r9\n"   
		"\tDATA_LOWER rdi\n"
		"\tshl rdi,30\n" 				;mone
		"\tmov rcx,1\n"  				;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r9,rdi\n"
		   
		"AFTER_ARG2_CHECKED_MUL:\n" 

		"PLUS_TWO_ARGS_FRACTION_MUL:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9\n"
		"\tDATA_UPPER_MEIR r10\n" ; r10 = mone  arg 1 
		"\tDATA_UPPER_MEIR r11\n"; r11 = mone  arg 2 
		"\tmov r12, r8\n"
		"\tmov r13, r9\n"
		"\tDATA_LOWER_MEIR r12 \n";  r12 = mehane  arg 1
		"\tDATA_LOWER_MEIR r13  \n"; r12 = mehane  arg 2

		"\tmov rax, r10\n"
		"\tcqo\n"
		"\timul r11 \n" 	
		"\tmov r15, rax \n"   ; (mehane arg 2 * mone arg 1) = r15 

		"\tmov rax, r12\n"
		"\tcqo\n"
		"\timul r13 \n"
		"\tmov r8, rax \n"   ; (mehane arg 1 * mone arg 2 ) = r8

		

		"\tpush r8  \n"  ;  mehane of new fraction
		"\tpush r15\n"  ;   mone of new fraction
		"\tcall gcd\n"

		"\tmov r9, rax\n" ;  gcd result in r9 [reuse of r9  beacause i dont need the orginal value anymore]
		"\tpop r15\n"
		"\tpop r8\n"
		"\tmov rax, r15\n"
		"\tcqo\n"
		"\tidiv r9  \n"  
		"\tmov r15, rax \n" ; r15 = (mona / gcd)

		"\tmov rax, r8\n"
		"\tcqo\n"
		"\tidiv r9\n"
		"\tmov r8, rax  \n" ; r8 = (maehane / gcd)
		"\tshl r15,30 \n";   r15 is mone 
		"\tor r15, r8  \n"; with "or" turn r8 to mehane 

		"\tshl r15, TYPE_BITS \n"; change the type   
		"\tor r15, T_FRACTION\n"
		

		"CHECK_REDUCE_TO_INTEGER_MUL:\n"
		"\tmov r8, r15\n"

		"\tDATA_LOWER_MEIR r8 \n" ;mehane
		"\tmov r9,r15\n"
		"\tDATA_UPPER_MEIR r9  \n"  ;mone

		"\tmov rax,r9\n"
		"\tmov rdx,0\n"
		"\tcqo\n"
		"\tidiv r8\n"

		"\tcmp rdx,0    \n" 	; rdx = remainder
		"\tje Binary_mul_CHANGE_TO_INTEGER\n"
		"\tmov rax, r15\n"
		"\tjmp Binary_mul_END\n"

		"Binary_mul_NO_PARAMETERS:\n"
		"\tmov r9, 1\n"
		"\tshl r9, TYPE_BITS\n"
		"\tor r9, T_INTEGER\n"
		"\tmov rax, r9\n"
		"\tjmp Binary_mul_END\n"

		"Binary_mul_CHANGE_TO_INTEGER:\n"
		"\tshl rax, TYPE_BITS\n"
		"\tor rax, T_INTEGER\n"

		"\tjmp Binary_mul_END\n"

		"Binary_mul_END:\n"
		"\tpop rbp\n"
		"\tret	\n"

		"Binary_mul_body:\n"
		"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , Binary_mul\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

	))))

(define lib_binary_div
	(lambda ()
		(let ((label (get_fvar_label '/)))
		(string-append

		"\tjmp Binary_div_body\n"

		"Binary_div:\n"
		"\tpush rbp\n"
		"\tmov rbp,rsp\n"
		"\tmov r8, arg_count\n"
		"\tcmp r8,1 \n"
		"\tjne Binary_div_REGULAR \n"

		"Binary_div_ONE_PARAMETER:\n"
		"\tmov r8, 1\n"
		"\tmov r9 , An(0)\n"
		"\tjmp Base_condition\n"

		"Binary_div_REGULAR:\n"
		"\tmov r8 , 0\n"
		"\tmov r8, An(0)\n"
		"\tmov r9, An(1)\n"

		"AFTER_CONDITIONS_OF_PARAMETERS_DIV:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9 \n"; transfer the args to temp register
		"\tand r10, 15\n" ; put type of r8 in r10
		"\tand r11, 15\n" ; put type of r9 in r11

		"\tcmp r10, T_INTEGER\n" 		;check if first arg is intger 
		"\tje CONVERT_ARG1_TO_FRACTION_DIV\n"
			 
		"\tjmp AFTER_ARG1_CHECKED_DIV\n"


		"CONVERT_ARG1_TO_FRACTION_DIV:\n"
		"\tmov rdi,r8\n"   			
		"\tDATA_LOWER_MEIR rdi\n"
	  	"\tshl rdi,30\n"		 ;mone
		"\tmov rcx,1\n"  		;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r8,rdi\n"

		"AFTER_ARG1_CHECKED_DIV:\n"
		"\tcmp r11, T_INTEGER\n" ; check intger of first arg
		"\tje CONVERT_ARG2_TO_FRACTION_DIV\n"
			 
		"\tjmp AFTER_ARG2_CHECKED_DIV\n"

		"CONVERT_ARG2_TO_FRACTION_DIV:\n"
		"\tmov rdi,r9\n"   
		"\tDATA_LOWER_MEIR rdi\n"
		"\tshl rdi,30\n" 				;mone
		"\tmov rcx,1\n"  				;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r9,rdi\n"
		   
		"AFTER_ARG2_CHECKED_DIV:\n" 

		"MUL_TWO_ARGS_FRACTION_DIV:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9\n"
		"\tDATA_UPPER_MEIR r10\n" ; r10 = mone  arg 1 
		"\tDATA_UPPER_MEIR r11\n"; r11 = mone  arg 2 
		"\tmov r12, r8\n"
		"\tmov r13, r9\n"
		"\tDATA_LOWER_MEIR r12 \n";  r12 = mehane  arg 1
		"\tDATA_LOWER_MEIR r13  \n"; r12 = mehane  arg 2

		"\tmov rax, r10\n"
		"\tcqo\n"
		"\timul r13 \n" 	
		"\tmov r15, rax \n"   ; (mehane arg 2 * mone arg 1) = r15 

		"\tmov rax, r12\n"
		"\tcqo\n"
		"\timul r11 \n"
		"\tmov r8, rax \n"   ; (mehane arg 1 * mone arg 2 ) = r8

		

		"\tpush r8  \n"  ;  mehane of new fraction
		"\tpush r15\n"  ;   mone of new fraction
		"\tcall gcd\n"

		"\tmov r9, rax\n" ;  gcd result in r9 [reuse of r9  beacause i dont need the orginal value anymore]
		"\tpop r15\n"
		"\tpop r8\n"
		"\tmov rax, r15\n"
		"\tcqo\n"
		"\tidiv r9  \n"  
		"\tmov r15, rax \n" ; r15 = (mona / gcd)

		"\tmov rax, r8\n"
		"\tcqo\n"
		"\tidiv r9\n"
		"\tmov r8, rax  \n" ; r8 = (mehane / gcd)
		

		"\tcmp r8, 0\n" ; mehana cant be neg
		"\tjg NO_NEG_MEHANE\n" ;;MABY MISTAK? 

		"NEG_MEHANE:\n"
		"\tneg r8\n"
		"\tneg r15\n"

		"NO_NEG_MEHANE:\n"
		"\tshl r15,30 \n";   r15 is mone 
		"\tor r15, r8  \n"; with "or" turn r8 to mehane 

		"\tshl r15, TYPE_BITS \n"; change the type   
		"\tor r15, T_FRACTION\n"
		

		"CHECK_REDUCE_TO_INTEGER_DIV:\n"
		"\tmov r8, r15\n"

		"\tDATA_LOWER_MEIR r8 \n" ;mehane
		"\tmov r9,r15\n"
		"\tDATA_UPPER_MEIR r9  \n"  ;mone

		"\tmov rax,r9\n"
		"\tmov rdx,0\n"
		"\tcqo\n"
		"\tidiv r8\n"

		"\tcmp rdx,0    \n" 	; rdx = remainder
		"\tje Binary_div_CHANGE_TO_INTEGER\n"
		"\tmov rax, r15\n"
		"\tjmp Binary_div_END\n"


		"Binary_div_CHANGE_TO_INTEGER:\n"
		"\tshl rax, TYPE_BITS\n"
		"\tor rax, T_INTEGER\n"

		"\tjmp Binary_div_END\n"
		;*******************************************************
		"Base_condition:\n"
		"\tmov r11, r9\n"
		"\tand r11, 15\n" ;check if the number is integer or fraction
		"\tcmp r11, T_INTEGER\n"
		"\tje return_frac\n"
		"\tmov r8, 1\n"
		"\tshl r8, TYPE_BITS\n"
		"\tor r8, T_INTEGER\n"
		"\tjmp AFTER_CONDITIONS_OF_PARAMETERS_DIV\n"


		"return_frac:\n"
		"\tcmp r9, 0\n"
		"\tjg return_pos_frac\n"
		"\tneg r8\n"
		"\txor rcx, rcx\n"
		"\tDATA r9\n"
		"\tmov r15, r9\n"
		"\tsub rcx, r15\n"
		"\tmov r9, rcx\n"
		"\tshl r9, TYPE_BITS\n"
		"\t or r9, T_INTEGER\n"
		

		"return_pos_frac:\n"
		"\tmov rdi,r8\n"   ;mone
		"\tmov rcx,r9\n"   ;meane
		"\tDATA_LOWER_MEIR rcx\n"
		"\tshl rdi, 30\n"
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		;"\tjmp CHECK_REDUCE_TO_INTEGER_DIV\n";;*********************
		"\tmov rax,rdi\n" ;new fraction


		"Reduce_Fraction:\n"
		"\tmov r13, rax\n"
		"\tDATA_LOWER_MEIR r13 \n" ;mehane
		"\tmov r14,rax\n"
		"\tDATA_UPPER_MEIR r14\n" ;mone
		"\tmov rax, r14\n"
		"\tmov rdx, 0\n"
		"\tcqo\n"
		"\tidiv r13\n"
		"\tcmp rdx, 0\n"
		"\tje Binary_change\n"
		"\tmov rax, rdi\n"
		"\tjmp Binary_div_END\n"

		"Binary_change:\n"
		"\tshl rax, TYPE_BITS\n"
		"\tor rax, T_INTEGER\n"

		"Binary_div_END:\n"
		"\tpop rbp\n"
		"\tret	\n"

		"Binary_div_body:\n"
		"\t mov rbx , 0\n"
		"\tMY_MALLOC(16)\n"
		"\tMAKE_LITERAL_CLOSURE rax, rbx , Binary_div\n"
		"\tmov rax, [rax]\n"
		"\tmov qword ["label"], rax\n"

	))))

(define lib_binary_equal
	(lambda ()
		(let ((label (get_fvar_label '=)))
		(string-append

		"\tjmp Binary_equal_body\n"

		"Binary_equal:\n"
		"\tpush rbp\n"
		"\tmov rbp,rsp\n"
		"\tmov r8, arg_count\n"
		"\tcmp r8,1 \n"
		"\tje equal_TRUE \n"

		"\tmov r8, An(0)\n"
		"\tmov r9, An(1)\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9 \n"; transfer the args to temp register
		"\tand r10, 15\n" ; put type of r8 in r10
		"\tand r11, 15\n" ; put type of r9 in r11

		"\tcmp r10, T_INTEGER\n" 		;check if first arg is intger 
		"\tje CONVERT_ARG1_TO_FRACTION_EQUAL\n"
			 
		"\tjmp AFTER_ARG1_CHECKED_EQUAL\n"


		"CONVERT_ARG1_TO_FRACTION_EQUAL:\n"
		"\tmov rdi,r8\n"   			
		"\tDATA_LOWER_MEIR rdi\n"
	  	"\tshl rdi,30\n"		 ;mone
		"\tmov rcx,1\n"  		;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r8,rdi\n"

		"AFTER_ARG1_CHECKED_EQUAL:\n"
		"\tcmp r11, T_INTEGER\n" ; check intger of first arg
		"\tje CONVERT_ARG2_TO_FRACTION_EQUAL\n"
			 
		"\tjmp AFTER_ARG2_CHECKED_EQUAL\n"

		"CONVERT_ARG2_TO_FRACTION_EQUAL:\n"
		"\tmov rdi,r9\n"   
		"\tDATA_LOWER rdi\n"
		"\tshl rdi,30\n" 				;mone
		"\tmov rcx,1\n"  				;mahane
		"\tor rdi,rcx\n" 
		"\tshl rdi, TYPE_BITS\n"
		"\tor rdi, T_FRACTION\n"
		"\tmov r9,rdi\n"
		   
		"AFTER_ARG2_CHECKED_EQUAL:\n" 

		"PLUS_TWO_ARGS_FRACTION_EQUAL:\n"
		"\tmov r10, r8\n"
		"\tmov r11, r9\n"
		"\tDATA_UPPER_MEIR r10\n" ; r10 = mone  arg 1 
		"\tDATA_UPPER_MEIR r11\n"; r11 = mone  arg 2 
		"\tmov r12, r8\n"
		"\tmov r13, r9\n"
		"\tDATA_LOWER_MEIR r12 \n";  r12 = mehane  arg 1
		"\tDATA_LOWER_MEIR r13  \n"; r12 = mehane  arg 2

		"\tmov rax, r10\n"
		"\tcqo\n"
		"\timul r13 \n" 	
		"\tmov r15, rax \n"   ; (mehane arg 2 * mone arg 1) = r15 

		"\tmov rax, r11\n"
		"\tcqo\n"
		"\timul r12 \n"
		"\tmov r14, rax \n"   ; (mehane arg 1 * mone arg 2 ) = r14

		"\tcmp r15, r14\n"  ; now r15 is the result of adding multiplactions- (the mone of the new fraction) 
		"\tje equal_TRUE\n"

		"equal_FALSE:\n"
		"\tmov rax, SOB_FALSE\n"
		"\tjmp Binary_equal_END\n"

		"equal_TRUE:\n"
		"\tmov rax,SOB_TRUE\n"

		"Binary_equal_END:	\n"
		"\tpop rbp\n"
		"\tret	\n"

		"Binary_equal_body:\n"
		"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , Binary_equal\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

	))))

(define lib_numerator
	(lambda ()
		(let ((label (get_fvar_label 'numerator)))
			(string-append

			"\tjmp make_numerator_body\n"
			"\tmake_numerator:\n"
			"\tpush rbp\n"
			"\tmov rbp,rsp\n"
			"\tmov r9,An(0)\n"
			"\tmov r10 , r9\n"
			"\tand r10, 15\n"
			"\tcmp r10, T_INTEGER\n"
			"\tje numerator_END\n"
			"\tDATA_UPPER_MEIR r9\n"
			"\tshl r9, TYPE_BITS\n"
			"\tor r9, T_INTEGER\n"
			"\tjmp numerator_END\n"

			"numerator_END:\n"
			"\tmov rax, r9\n"
			"\tpop rbp\n"
			"\tret\n"

			"make_numerator_body:\n"
			"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_numerator\n"
			"\tmov rax, [rax]\n"
			"\tmov ["label"], rax\n"

			))))

(define lib_denominator
	(lambda ()
		(let ((label (get_fvar_label 'denominator)))
			
			(string-append
			 
			"\tjmp make_denominator_body\n"
			"\tmake_denominator:\n"
			"\tpush rbp\n"
			"\tmov rbp,rsp\n"
			"\tmov r9,An(0)\n"
			"\tmov r10 , r9\n"
			"\tand r10, 15\n"
			"\tcmp r10, T_INTEGER\n"
			"\tje integer_denominator\n"
			"\tDATA_LOWER r9\n"
			"\tshl r9, TYPE_BITS\n"
			"\tor r9, T_INTEGER\n"
			"\tjmp denominator_END\n"
			
			"integer_denominator:\n"
			"\tmov r9, 1\n"
			"\tshl r9, TYPE_BITS\n"
			"\tor r9, T_INTEGER\n"

			"denominator_END:\n"
			"\tmov rax, r9\n"
			"\tpop rbp\n"
			"\tret\n"

			"make_denominator_body:\n"
			"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_denominator\n"
			"\tmov rax, [rax]\n"
			"\tmov ["label"], rax\n"

			))))


(define lib_cons 
	(lambda ()
		(let ((label (get_fvar_label 'cons)))
		(string-append	
			"\tjmp make_cons_body\n"

			"\tmake_cons:\n"
		 	"\tpush rbp\n"
			"\tmov rbp, rsp\n"

		 	"\tmov r9, An(0)\n"
		 	"\tmov r8, An(1)\n"
		 	"\tMY_MALLOC(8)\n"
		 	"\tmov r11 , rax\n"  ;MY_MALLOC return pointer in rax 
		 	"\tmov [r11] , r9\n" ; we put in the value of malloc first elemen
		 	"\tMY_MALLOC(8)\n"
		 	"\tmov r10,rax\n" ;MY_MALLOC return pointer in rax 
		 	"\tmov [r10], r8\n" ; we put in the value of malloc second element
		 	"\tMY_MALLOC(8)\n"
		 	"\tMAKE_MALLOC_LITERAL_PAIR rax , r11 ,r10\n"
		 	"\tmov rax, qword [rax]\n"

			"\tpop rbp\n"
		 	"\tret\n" 



		 	"make_cons_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_cons\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
		))))

(define lib_remainder
	(lambda ()
		(let ((label (get_fvar_label 'remainder)))
			(string-append
			"\tjmp make_remainder_body\n"
			"\tmake_remainder:\n" ; in remainder in scheme the argument need to be integers 
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r10, An(0)\n" ; first arg
			"\tmov r11,An(1)\n" ; second arg 

			"\tDATA r10\n"
			"\tDATA r11\n"
			"\tmov rax, r10\n" 
			"\tmov rdx,0\n"
			"\tcqo\n"
			"\tidiv r11\n"  ; after the div the remain in rdx 
			"\tmov r10,rdx \n" ; mov the remain to r10
			"\tshl r10, TYPE_BITS\n"
			"\tor r10, T_INTEGER\n"
			"\tmov rax, r10\n"
		 	"\tpop rbp\n"
		 	"\tret \n"


		 	"make_remainder_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_remainder\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
		))))

(define lib_char_to_integer
	(lambda ()
		(let ((label (get_fvar_label 'char->integer)))
			(string-append
			"\tjmp char_to_integer_body\n"
			"\tchar_to_integer:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r9, An(0)\n"
			"\tDATA r9\n"
			"\tshl r9, TYPE_BITS\n"
			"\tor r9, T_INTEGER\n"
			"\tmov rax, r9\n"
			"\tpop rbp\n"
			"\tret\n"

			"char_to_integer_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , char_to_integer\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_integer_to_char
	(lambda ()
		(let ((label (get_fvar_label 'integer->char)))

			(string-append
				
			"\tjmp integer_to_char_body\n"
			"integer_to_char:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r8, An(0)\n"
			"\tDATA r8\n"
			"\tshl r8, TYPE_BITS\n"
			"\tor r8, T_CHAR\n"
			"\tmov rax, r8\n"
			"\tpop rbp\n"
			"\tret\n"

			"integer_to_char_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , integer_to_char\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_string_length 
	(lambda ()
		(let ((label (get_fvar_label 'string-length)))
			(string-append
			"\tjmp string_length_body\n"
			
			"make_string_length:\n"
			"\tpush rbp\n"
		 	"\tmov rbp, rsp\n"
		 	"\tpush r15\n"  ; save the register of define very important 
		 	"\tmov r15, An(0)\n" ; the first element in stack
		 	"\tSTRING_LENGTH r15\n"  ; return the length
			"\tshl r15, TYPE_BITS\n"
			"\tor r15, T_INTEGER\n"  ; transform to int in run time 
			"\tmov rax, r15\n"
			"\tpop r15 \n"   ; save the register of define very important
		 	"\tpop rbp\n"
		 	"\tret\n"

			"string_length_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_string_length\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

			))))

(define lib_string_ref
	(lambda ()
		(let ((label (get_fvar_label 'string-ref)))
			(string-append

			"\tjmp string_ref_body\n"
			"\tmake_string_ref:\n"
		 	"\tpush rbp\n"
		 	"\tmov rbp, rsp\n"
		 	"\tmov r14 , An(0)\n" ; the string 
			"\tmov r12, An(1)\n" ; the index
			"\tDATA r12\n" ; we want the number not pointer
			"\tSTRING_REF bl, r14, r12  \n" ; the meir implenation of STRING_REF in bl will be the result beacause mayer make move byte in the macro
			"\tshl rbx, TYPE_BITS \n"; the result in bl so that we make place to the TYPE (char)
			"\tor rbx, T_CHAR\n"  ; transform to char in run time
			"\tmov rax, rbx\n"
			"\tpop rbp\n"
			"\tret\n"


			"string_ref_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_string_ref\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

			))))

(define lib_string_set
	(lambda () 
		(let ((label (get_fvar_label 'string-set!)))
			(string-append

			"\tjmp string_set_body\n"
			"make_string_set:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"
			"\tmov r13, An(0)\n" ; first element the string 	
			"\tmov r15, An(1)\n" ; second element the index
			"\tmov r14, An(2)\n" ; third element the char i want to replace 
			 	
			"\tDATA r15\n"
		 	"\tDATA r14	\n"
		 	"\tSTRING_ELEMENTS r13\n"
		 	"\tadd r13, r15\n"
			"\tmov rax, r13\n"
			"\tmov byte [rax], r14b \n"
		 	"\tmov rax, SOB_VOID\n"
		 	"\tpop rbp\n"
		 	"\tret\n"

			"string_set_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_string_set\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))


(define lib_symbol_to_string
	(lambda ()
		(let ((label (get_fvar_label 'symbol->string)))
			(string-append
			"\tjmp symbol_to_string_body\n"
			"\tsymbol_to_string:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"

			"\tmov r9, An(0)\n"
			"\tDATA r9\n"
			"\tadd r9 , start_of_data\n"
			"\tmov r10 , [r9] \n"
			"\tmov rax , r10 \n"
			"\tpop rbp\n"
			"\tret\n"

			"symbol_to_string_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , symbol_to_string\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))


(define lib_string_to_symbol
	(lambda ()
		(let ((label (get_fvar_label 'string->symbol)))
			(string-append
			"\tjmp string_to_symbol_body\n"

			"\tstring_to_symbol:\n"
			"\tpush rbp\n"
			"\tmov rbp, rsp\n"

			"\tmov r9, An(0)\n"
			
			"\tmov r13 , qword[symbol_table] \n" ;r13 has the adress that symbol table point to
			"\tcmp r13, SOB_NIL \n" 
			"\tje add_new_symbol_to_empty_table\n"

			"search_in_symbol_table:\n"
			"\tmov r14, [r13]\n"  ;r14 is the label of the symbol in the current pair - r13. 
			"\tDATA r14\n"   ;mabey not ?1
			"\tadd r14 , start_of_data\n" ;mabey not ?!
			"\tmov r11 , [r14]\n"  ;r11 has the string label of the symbol
			"\tcmp r11, r9\n" ;cmpare the string labels
			"\tje ret_the_symbol\n" ;equal - return the symbol 
			"\tmov r12 , [r13+8]\n"  ; adress of the next next pair 
			"\tcmp r12 , SOB_NIL \n" ;the current pair is the last pair , so create new symbol
			"\tje add_new_symbol_to_table\n"  
			"\tmov r13, r12\n" ; "inc" the pair
			"\tjmp search_in_symbol_table\n"

			"ret_the_symbol:\n"
			"\tmov rax, [r13]\n"  ;return the label of the symbol
			"\tjmp string_to_symbol_END\n"

			"add_new_symbol_to_table:\n"
			"\tMY_MALLOC (8)\n"   ; create symbol
            "\tmov r15, rax\n"
            "\tmov qword[r15] , r9 \n"
            "\tsub r15 , start_of_data \n"
            "\tshl r15 ,TYPE_BITS\n"
            "\tor r15 , T_SYMBOL\n"

            "\tMY_MALLOC(16)\n"  ;create pair to add to symbol table
            "\tmov r10 , rax\n"
            "\tmov [r10] , r15\n"
            "\tmov qword[r10+8] , SOB_NIL\n"

            "\tmov [r13+8] , r10\n"  ;change the last pair to point the new pair 
            "\tmov rax ,r15\n"
            "\tjmp string_to_symbol_END \n"

            "add_new_symbol_to_empty_table:\n"
			"\tMY_MALLOC (8)\n"   ; create symbol
            "\tmov r15, rax\n"
            "\tmov qword[r15] , r9 \n"
            "\tsub r15 , start_of_data \n"
            "\tshl r15 ,TYPE_BITS\n"
            "\tor r15 , T_SYMBOL\n"

            "\tMY_MALLOC(16)\n"  ;create pair to add to symbol table
            "\tmov r10 , rax\n"
            "\tmov [r10] , r15\n"
            "\tmov qword[r10+8] , SOB_NIL\n"
            ;"\tMAKE_MALLOC_LITERAL_PAIR r10 ,  r15  , SOB_NIL\n"
            "\tmov r12 , qword[symbol_table]\n"
            "\tmov r12 , r10\n"
            "\tmov rax ,r15\n"
            "\tjmp string_to_symbol_END \n"	

			"string_to_symbol_END:\n"
			"\tpop rbp\n"
			"\tret\n"

			"string_to_symbol_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , string_to_symbol\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))


(define lib_vector_length
	(lambda ()
		(let ((label (get_fvar_label 'vector-length)))
			(string-append
			"\tjmp vector_length_body\n"
			
			"make_vector_length:\n"
		 	"\tpush rbp\n"
		 	"\tmov rbp, rsp\n"
		 	"\tpush r15  \n"; save the register of define very important 
		 	"\tmov r15, An(0)\n"
		 	"\tVECTOR_LENGTH r15 \n" ; the meir implenation of VECTOR_LENGTH
			"\tshl r15, TYPE_BITS\n"
			"\tor r15, T_INTEGER \n" ; transform to int in run time 
			"\tmov rax, r15\n" 
			"\tjmp vector_length_END\n"
			
			"return_empty_vector:\n"
			"\tmov rax, 0\n"

			"vector_length_END:\n"
			"\tpop r15   \n"; save the register of define very important
		 	"\tpop rbp\n"
		 	"\tret\n"

			"vector_length_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_vector_length\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_vector_ref
	(lambda ()
		(let ((label (get_fvar_label 'vector-ref)))
			(string-append

			"\tjmp vector_ref_body\n"
			"make_vector_ref:\n"
			"\tpush rbp\n"
		 	"\tmov rbp, rsp\n"
			"\tmov r14 , An(0)\n" ; the vector 
			"\tmov r15, An(1)\n" ; the index
			"\tDATA r15\n" ; we want the number not pointer
			"\tVECTOR_REF r13, r14, r15\n"   ; the meir implenation of VECTOR_REF in r14 will be the result
			"\tmov rax, r13\n"
			"\tpop rbp\n"
			"\tret\n"

			"vector_ref_body:\n"
		 	"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_vector_ref\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_vector_set
	(lambda ()
		(let ((label (get_fvar_label 'vector-set!)))
			(string-append
				"\tjmp vector_set_body\n"
				
				"make_vector_set:\n"
			  	"\tpush rbp\n"
			 	"\tmov rbp, rsp\n"
			 	"\tmov r15, An(0)\n" ; first element of the vector 	
			 	"\tmov r12, An(1)\n" ; second element of the index
			 	"\tmov r13, An(2)\n" ; third element the object i want to replace 
			  	"\tDATA r12\n"  ; need the specific index so need the data
				"\tVECTOR_SET r10, r15, r12, r13\n"
			 	"\tmov rax, SOB_VOID\n"
			 	"\tpop rbp\n"
			 	"\tret\n"

				"\nvector_set_body:\n"
				"\t mov rbx , 0\n"
				"\tMY_MALLOC(16)\n"
				"\tMAKE_LITERAL_CLOSURE rax, rbx , make_vector_set\n"
				"\tmov rax, [rax]\n"
				"\tmov qword ["label"], rax\n"
			))))

(define lib_make_apply
	(lambda ()
		(let ((label (get_fvar_label 'apply)))
			(string-append

			"\tjmp apply_body\n"
			"make_apply:\n"

			 "\tpop r8\n" ;ret
			 "\tpop r9\n" ;env
			 "\tpop r9\n" ;n
			 "\tpop r10\n" ; operator
			 "\tpop r12\n" ;  start list

			"\tmov rdi,1\n"
			"\tmov r13,r12 \n"; the list 

			"Num_of_operands:\n"
			"\tCDR r13 \n"; point to value of cdr
			"\tcmp r13, SOB_NIL\n"
			"\tje Num_of_operands_EXIT\n"
			"\tadd rdi, 1\n"
			"\tjmp Num_of_operands\n"

			"Num_of_operands_EXIT:\n"
			"\tmov r15,rdi\n"
			"\tshl r15,3\n"
			"\tsub rsp , r15\n" ;rsp in the bootom of operands,put operands by the rsp 
			"\tmov r15,rsp \n"; put element
			"\tmov r13,r12 \n"; the list 

			"Push_operands_loop:\n"
			"\tmov r14,r13\n" ;save the list 
			"\tCAR r13 \n"; point to value of car
			"\tmov [r15],r13\n"
			"\tadd r15, 8\n"
			"\tCDR r14\n"
			"\tcmp r14, SOB_NIL\n"
			"\tje Update_stack\n"
			"\tmov r13,r14 \n";the next node
			"\tjmp Push_operands_loop\n"


			"Update_stack:\n"
			"\tpush rdi \n"; push new n 
			"\tmov r15,r10\n"
			"\tCLOSURE_ENV r15\n"
			"\tpush r15 \n" ;push  new env
			"\tpush r8\n" ; push ret
			"\tmov r15,r10\n"
			"\tCLOSURE_CODE r15\n"
			"\tjmp r15\n"

			"apply_body:\n"
			"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_apply\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"

			))))

(define lib_grater
	(lambda ()
		(let ((label (get_fvar_label '>)))
			(string-append

			"\tjmp grater_body\n"
			"make_grater:\n"
			"\tpush rbp\n"
			"\tmov rbp,rsp\n"
			"\tmov r15, arg_count\n"
			"\tcmp r15, 1\n"
			"\tje grater_True\n"
			"\tmov r8, An(0)\n"
			"\tmov r9, An(1)\n"
			"\tmov r10, r8\n"
			"\tmov r11, r9\n" ; and - transfer the args to temp
			"\tand r10, 15\n" 
			"\tand r11, 15\n" 


			"\tcmp r10, T_INTEGER\n" ; check if integer arg1
			"\tje CONVERT_ARG1_TO_FRACTION_grater\n"
			 
			"\tjmp AFTER_ARG1_CHECKED_grater\n"


			"CONVERT_ARG1_TO_FRACTION_grater:\n"
			"\t mov rdi,r8\n"   ;arg1 is integer
			"\tDATA_LOWER_MEIR rdi\n"
			"\tshl rdi,30\n" ;nominator
			"\tmov rcx,1\n"  ;dominator
			"\tor rdi,rcx\n" 
			"\tshl rdi, TYPE_BITS\n"
			"\tor rdi, T_FRACTION\n"
			"\tmov r8,rdi\n"


			"AFTER_ARG1_CHECKED_grater:\n"
			"\tcmp r11, T_INTEGER\n" ; check if integer arg2
			"\tje CONVERT_ARG2_TO_FRACTION_grater\n"
			"\tjmp AFTER_ARG2_CHECKED_grater\n"

			 "CONVERT_ARG2_TO_FRACTION_grater:\n"
			 "\tmov rdi,r9\n"   ;arg2 integer
			 "\tDATA_LOWER rdi\n"
			 "\tshl rdi,30\n" ;nominator
			 "\tmov rcx,1\n"  ;dominator
			 "\tor rdi,rcx\n" 
			 "\tshl rdi, TYPE_BITS\n"
			 "\tor rdi, T_FRACTION\n"
			 "\tmov r9,rdi\n"
				 
				  
			"AFTER_ARG2_CHECKED_grater:\n"
			"PLUS_TWO_ARGS_FRACTION_grater:\n"
			"\t	mov r10, r8\n"
			"\tmov r11, r9\n"
			"\tDATA_UPPER_MEIR r10\n" ;r10 = nominator of arg 1 
			"\tDATA_UPPER_MEIR r11\n";r11 = nominator of arg 2 
			"\tmov r12, r8\n"
			"\tmov r13, r9\n"
			"\tDATA_LOWER_MEIR r12\n" ; r12 = denominator of arg 1
			"\tDATA_LOWER_MEIR r13\n"  ; r13 = denominator of arg 2

			"\tmov rax, r10\n"; 
			"\tcqo\n"
			"\timul r13 \n"; multiply dominator arg2 nominator arg1 = rax 
			"\tmov r15, rax\n" ; result in r15 

			"\tmov rax, r11\n";  
			"\tcqo\n"
			"\timul r12\n" ; multiply denominator arg1 with nominator arg2 = rax 
			"\tmov r14, rax\n" ;result = r14
				
			"\tcmp r15, r14 \n"  ; compare for "mahane meshutaf" 
			"\tjg grater_True\n"

			"grater_False:\n"
			"\tmov rax, SOB_FALSE\n"
			"\tjmp grater_EXIT\n"


			"grater_True:\n"
			"\tmov rax, SOB_TRUE\n"


			"grater_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"
			"grater_body:\n"
			"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_grater\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define lib_smaller
	(lambda ()
		(let ((label (get_fvar_label '<)))
			(string-append

			"\tjmp smaller_body\n"
			"make_smaller:\n"
			"\tpush rbp\n"
			"\tmov rbp,rsp\n"
			"\tmov r15, arg_count\n"
			"\tcmp r15, 1\n"
			"\tje smaller_True\n"
			"\tmov r8, An(0)\n"
			"\tmov r9, An(1)\n"
			"\tmov r10, r8\n"
			"\tmov r11, r9\n" ; and - transfer the args to temp
			"\tand r10, 15\n" 
			"\tand r11, 15\n" 


			"\tcmp r10, T_INTEGER\n" ; check if integer arg1
			"\tje CONVERT_ARG1_TO_FRACTION_smaller\n"
			 
			"\tjmp AFTER_ARG1_CHECKED_smaller\n"


			"CONVERT_ARG1_TO_FRACTION_smaller:\n"
			"\t mov rdi,r8\n"   ;arg1 is integer
			"\tDATA_LOWER_MEIR rdi\n"
			"\tshl rdi,30\n" ;nominator
			"\tmov rcx,1\n"  ;dominator
			"\tor rdi,rcx\n" 
			"\tshl rdi, TYPE_BITS\n"
			"\tor rdi, T_FRACTION\n"
			"\tmov r8,rdi\n"


			"AFTER_ARG1_CHECKED_smaller:\n"
			"\tcmp r11, T_INTEGER\n" ; check if integer arg2
			"\tje CONVERT_ARG2_TO_FRACTION_smaller\n"
			"\tjmp AFTER_ARG2_CHECKED_smaller\n"

			 "CONVERT_ARG2_TO_FRACTION_smaller:\n"
			 "\tmov rdi,r9\n"   ;arg2 integer
			 "\tDATA_LOWER rdi\n"
			 "\tshl rdi,30\n" ;nominator
			 "\tmov rcx,1\n"  ;dominator
			 "\tor rdi,rcx\n" 
			 "\tshl rdi, TYPE_BITS\n"
			 "\tor rdi, T_FRACTION\n"
			 "\tmov r9,rdi\n"
				 
				  
			"AFTER_ARG2_CHECKED_smaller:\n"
			"PLUS_TWO_ARGS_FRACTION_smaller:\n"
			"\t	mov r10, r8\n"
			"\tmov r11, r9\n"
			"\tDATA_UPPER_MEIR r10\n" ;r10 = nominator of arg 1 
			"\tDATA_UPPER_MEIR r11\n";r11 = nominator of arg 2 
			"\tmov r12, r8\n"
			"\tmov r13, r9\n"
			"\tDATA_LOWER_MEIR r12\n" ; r12 = denominator of arg 1
			"\tDATA_LOWER_MEIR r13\n"  ; r13 = denominator of arg 2

			"\tmov rax, r10\n"; 
			"\tcqo\n"
			"\timul r13 \n"; multiply dominator arg2 nominator arg1 = rax 
			"\tmov r15, rax\n" ; result in r15 

			"\tmov rax, r11\n";  
			"\tcqo\n"
			"\timul r12\n" ; multiply denominator arg1 with nominator arg2 = rax 
			"\tmov r14, rax\n" ;result = r14
				
			"\tcmp r15, r14 \n"  ; compare for "mahane meshutaf" 
			"\tjl smaller_True\n"

			"smaller_False:\n"
			"\tmov rax, SOB_FALSE\n"
			"\tjmp smaller_EXIT\n"


			"smaller_True:\n"
			"\tmov rax, SOB_TRUE\n"


			"smaller_EXIT:\n"
			"\tpop rbp\n"
			"\tret\n"

			"smaller_body:\n"
			"\t mov rbx , 0\n"
			"\tMY_MALLOC(16)\n"
			"\tMAKE_LITERAL_CLOSURE rax, rbx , make_smaller\n"
			"\tmov rax, [rax]\n"
			"\tmov qword ["label"], rax\n"
			))))

(define const_table_to_ass
	(lambda ()
        (let ((i (- START_MEMORY 1)))
            (fold-left
                string-append
                (string-append
                
                	"section .data \n "
					"start_of_data:" " \n"
					"sobVoid: \n" "\t dq SOB_VOID \n"
					"sobNil: \n" "\t dq SOB_NIL \n"
					"sobFalse: \n" "\t dq SOB_FALSE \n"
					"sobTrue: \n" "\t dq SOB_TRUE \n"  )

                (cMap (lambda (row)
                	(if (list? row)
                	        (let ((label (caaddr row))
                	        		(item (cadr  row)))
                        		(cond 
                        			((equal? "sobInt" (substring label 0 6)) 
                        				(string-append label ": \n" "\t dq MAKE_LITERAL(T_INTEGER, " (to_string item) ") \n"))
                        			((equal? (string-length label) 6) "")
                        			((equal? "sobChar" (substring label 0 7))
                        				(let* ((char_label (helper_label item))) 
                        					(string-append label ": \n" "\t dq MAKE_LITERAL(T_CHAR, "char_label ") \n")))
                        			((equal? "sobFrac" (substring label 0 7))
                        				(string-append label ": \n" "\t dq MAKE_LITERAL_FRACTION(" (cadr(caddr row)) "," (caddr (caddr row)) ") \n"))
                        			((equal? "sobPair" (substring label 0 7))
                        				(string-append label ": \n" "\t dq MAKE_LITERAL_PAIR(" (cadr(caddr row)) "," (caddr (caddr row)) ") \n"))
                        			((or (equal? (string-length label) 7) (equal? (string-length label) 8)) "")
                        			((equal? "sobString" (substring label 0 9))
                        				(string-append label ": \n" "\t MAKE_LITERAL_STRING  \"" item "\"\n") )
                        			((equal? "sobSymbol" (substring label 0 9))
                        				(let ((strLabel (get_label (to_string item) const_table ))) 
                        				(string-append label ": \n" "\t dq MAKE_LITERAL_SYMBOL(" strLabel ")\n") ))
                        			((equal? "sobVector" (substring label 0 9)) 
                        				(if (= (vector-length item) 0)
                        					(string-append label ": \n" "\t" "dq MAKE_LITERAL(T_VECTOR," (number->string 0) ")\n" )
                        					(string-append label ": \n" "\t" "MAKE_LITERAL_VECTOR " (get_vector_labels (cdr (cdaddr row))) " \n")))
                        			(else "ERROR TABLE ASSEMBLY")
                        		)
                        )
                        ""))
                    const_table))))
)

(define helper_label
	(lambda (item)
		(cond ((eq? item #\nul) "CHAR_NUL")
			  ((eq? item #\newline) "CHAR_NEWLINE")
			  ((eq? item #\tab) "CHAR_TAB")
			  ((eq? item #\page) "CHAR_PAGE")
			  ((eq? item #\return)"CHAR_RETURN")
			  ((eq? item #\space) "CHAR_SPACE")
			(else (to_string (char->integer item))))
	) 
)


(define get_vector_labels
	(lambda (lst)
		(if (null? lst) 
			(string-append "")
			(letrec ((loop 
				(lambda (lst)
					(if (null? (cdr lst))
						(string-append (car lst))
						(string-append (car lst) ", " (loop (cdr lst)))
					)
				)))
			(loop lst))
		)
	)
)

(define defult_symbol_table_to_ass
	(lambda ()
		(string-append
			"symbol_table: \n"
			"\tdq SOB_NIL\n"
		)
	)
)




(define symbol_table_to_ass
    (lambda ()
    (string-append
    	"" 
        (if (not (equal? symbol_table '()))
           (string-append
           	"test1:\n"
            "\tMY_MALLOC(16)\n" ;create first pair
            "\tmov r8, rax\n"  ;address first pair
            "\tmov r9 , qword[symbol_table]\n"  ;symbol table point
            "\tmov r9 , r8\n"  ;symbol table point to adress firat pair
            ;"\tmov [r8+8] , SOB_NIL\n "
            "\tpush r8\n" ;push adress to the stack
           (letrec ((loop 
                        (lambda (symLst)
                        (if (null? (cdr symLst))
                            (string-append 
                                "\tpop r8\n"
                                "\tmov qword[r8] , " (car symLst)" \n" 
                                "\tmov qword[r8 + 8] , SOB_NIL\n" 
                            )
                            (string-append
                                "\tpop r8\n" ;adress current node - pop from the stack
                                "\tmov qword[r8] , " (car symLst)" \n"
                                "\tMY_MALLOC (16)\n" ;create next pair
                                "\tmov r9, rax\n" ; ;r9 have the adress for next pair
                                "\tmov [r8 +8] , r9\n"
                               ; "\tMAKE_MALLOC_LITERAL_PAIR r8 , " (car symLst)  " , r9 \n" 
                                "\tpush r9\n"  ; enter the next node to the stack
                                (loop (cdr symLst))
                            )

                        ))))
           (loop symbol_table)
        ))
       ""))
    )
)

(define global_table_to_ass
	(lambda ()
        (fold-left
            string-append
            ""
            (cMap (lambda (row)
            	(if (list? row)
                    (string-append
                    	(cadr row) ":\n"
                        "\t dq SOB_UNDEFINED \n")
                ""))
                  global_table)))
)

(define to_string
    (lambda (exp)
        (letrec ((loop
        	(lambda (arg) 
        		(cond 
        			 ((null? arg) "")
        			 ((number? arg) (number->string arg))
              		 ((symbol? arg) (symbol->string arg))
              		 ((char? arg) (number->string (char->integer arg)))
             		  ((list? arg)  (string-append (loop (car arg)) (loop (cdr arg))))
              (else arg)))))
        (loop exp)
    )))

(define TEST_tables
	(lambda (exp)
		(constract_lists exp)
		(constract_tables)
		;const_table
		(constract_assembly_table)
	)
)

 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; CODE GENERATION ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define count_label_if 0)
(define count_label_or 0)
(define cgen_sub_prog 0)
(define counter_lambda 0)
(define counter_applic 0)
(define counter_lambda_opt 0)
(define counter_Tc-applic 0)

(define get_label_counter
    (lambda (counter)
    	 (cond
    		((equal? counter 'count_label_if) (set! count_label_if (+ count_label_if 1)) (number->string count_label_if))
    		((equal? counter 'count_label_or) (set! count_label_or (+ count_label_or 1)) (number->string count_label_or))
    		((equal? counter 'count_label_lambda) (set! counter_lambda (+ counter_lambda 1)) (number->string counter_lambda))
    		((equal? counter 'counter_label_lambda_opt) (set! counter_lambda_opt (+ counter_lambda_opt 1)) (number->string counter_lambda_opt))
    		((equal? counter 'counter_label_applic) (set! counter_applic (+ counter_applic 1)) (number->string counter_applic))
    		((equal? counter 'counter_label_Tc-applic) (set! counter_Tc-applic (+ counter_Tc-applic 1)) (number->string counter_Tc-applic))
    		(else (string-append "no_such_counter : " counter))) 
            ))


(define env -1)

(define code_gen 
	(lambda (pe env)
			(cond 
				((itIs? 'const pe) (let ((label (get_label (cadr pe) const_table )))
										(string-append "\tmov rax , [" label "]\n")))
				((itIs? 'seq pe) (code_gen_seq (cadr pe) env))
				((itIs? 'if3 pe) (code_gen_if (cdr pe) (get_label_counter 'count_label_if) env))
				((itIs? 'or pe) (code_gen_or (cadr pe) (get_label_counter 'count_label_or) env))
				((itIs? 'pvar pe) (let ((minor (number->string (caddr pe))))
										(string-append "\tmov rax , qword [rbp + (4 + " minor ") * 8 ]\n")))
				((itIs? 'bvar pe) (code_gen_bvar (cdr pe) env))
				((itIs? 'fvar pe) (code_gen_fvar (cdr pe) env))
				((itIs? 'define pe) (code_gen_def (cdr pe) env))
				((itIs? 'set pe) (code_gen_set (cdr pe) env))
				((itIs? 'lambda-simple pe) (code_gen_lambda_simple (cdr pe) (get_label_counter 'count_label_lambda) (+ env 1)) )
				((itIs? 'lambda-opt pe) (code_gen_lambda_opt (cdr pe) (get_label_counter 'counter_label_lambda_opt) (+ env 1)))
				((itIs? 'applic pe) (code_gen_applic (cdr pe) (get_label_counter 'counter_label_applic) env))
				((itIs? 'tc-applic pe) (code_gen_tc_applic (cdr pe) (get_label_counter 'counter_label_Tc-applic) env))
				((itIs? 'box pe) (code_gen_box (cdr pe) env))
				((itIs? 'box-set pe) (code_gen_box_set (cdr pe) env))
				((itIs? 'box-get pe) (code_gen_box_get (cdr pe) env))
			
				(else error "stop!_not_in_code_gen_:\n")
		)
	)
)

(define itIs? 
	(lambda (form exp)
		(if (pair? exp)
			(equal? (car exp) form)
			#f
		)
	)
)

(define code_gen_seq
	(lambda (expLst env)
		(letrec ((loop
				(lambda (lst env)
					(if (equal? (cdr lst) '())
						(code_gen (car lst) env); the answer need to be in rax , probably there. 
						(string-append (code_gen (car lst) env) (loop (cdr lst) env)))
				))) 
	(loop expLst env))
	)
) 

(define code_gen_if
	(lambda (lst counter env)
		(let ((test (car lst))
				(dit (cadr lst))
				(dif (caddr lst)))
			(string-append 
				(code_gen test env) "\n"
				"\tcmp rax , SOB_FALSE" "\n"
				"\tje L_if_false_" counter "\n"
				(code_gen dit env) "\n"
				"\tjmp L_if_end_" counter "\n"
				"L_if_false_" counter": \n"
				(code_gen dif env) "\n"
				"L_if_end_" counter ": \n"
			)
		)
	) 
)

(define code_gen_or
	(lambda (lst counter env)

		 (string-append
		 	(code_gen (car lst) env)
                (fold-right
                    string-append
                    ""
                    (map (lambda (el)
                            (string-append "\tcmp rax , SOB_FALSE\n"
                                        "\tjne L_or_exit_" counter"\n"
                                        (code_gen el env)))
                        (cdr lst)))
                "L_or_exit_" counter ":\n"
		)
	) 
) 

(define code_gen_bvar 
	(lambda (lst env)
		(let ((param (car lst))
				(ma (number->string (* 8 (cadr lst))))
				(mi (number->string (* 8 (caddr lst)))))
			(string-append
				"\tmov rax , qword [rbp + 2*8] \n"
				"\tmov rax , qword [rax + " ma " ] \n" 
				"\tmov rax , qword [rax + "  mi "] \n"  
			)
		)
	)
)

(define code_gen_fvar
	(lambda (e env)
		;(display e)
		(let ((label (get_fvar_label (car e))))
			(string-append
			;"Label_fvar:\n"
			"\tmov rax , qword [" label "] \n"
			)
		)
	)
)

(define code_gen_def
	(lambda (lst env)
		(let* ((v (cadar lst))
			  (defination (cadr lst))
			  (label (get_fvar_label v)))
			(string-append 
				(code_gen defination env)
				;"Label_define:\n"
				"\tmov rbx , " label " \n"
				"\tmov [rbx] , rax \n"
				"\tmov rax , SOB_VOID \n"				
			)
		)

	)
)

(define code_gen_set 
	(lambda (exp env)
		(let ((first (car exp))
			  (second (cadr exp)))
			(cond 
				((equal? (car first) 'pvar)
					(let ((mi (number->string (caddr first))))
						(string-append
							;"Label_set_pvar:\n"
							(code_gen second env)
							"\tmov qword[rbp + (4 + " mi ") * 8 ] , rax\n"
							"\tmov rax , SOB_VOID \n"
						)
					))
				((equal? (car first) 'bvar)
					(let ((ma (number->string (caddr first)))
						  (mi (number->string (cadddr first))))
						(string-append 
							;"Label_set_bvar:\n"
							(code_gen second env)
							"\tmov rbx , qword[rbp + 2 * 8] \n"
							"\tmov rbx , qword[rbx + " ma " * 8] \n"
							"\tmov qword[rbx + " mi " * 8 ] , rax \n"
							"\tmov rax , SOB_VOID \n"
						)
					))
				((equal? (car first) 'fvar)
					(let ((label (get_fvar_label (cadr first))))
						(string-append
							;"Label_set_fvar:\n"
							(code_gen second env)
							"\tmov ["label "], rax\n"
							"\tmov rax, SOB_VOID\n"
						)
				))
				(else "error : code gen set impossible")
			)
		)
	)
)

(define code_gen_box
	(lambda (var env)
		(string-append
			(code_gen (car var) env)
			"\tmov rbx, rax\n" ;save var
			"\tMY_MALLOC(8)\n" ;create place to pointer
			"\tmov qword[rax], rbx\n" ;wrap var in the pointer
		)
	)
)
(define code_gen_box_set
	(lambda (lst env)
			(let ((var_exp (car lst))
				  (value_exp (cadr lst)))
				(string-append
					(code_gen value_exp env) ;get value
					"\tmov rbx, rax\n";save the value from rax to rbx
					(code_gen var_exp env) ;get the box
					"\tmov [rax], rbx\n"; do the set
					"\tmov rax, SOB_VOID\n";update rax to void
				)
			)
	)
)

(define code_gen_box_get
	(lambda (lst env)
		(string-append 
			(code_gen (car lst) env)
			"\tmov rax, [rax]\n"
		)
	)
)

(define code_gen_lambda_simple 
	(lambda (lst counter env)
		(let ((paramLst (car lst))
			  (body (cadr lst)))
			 (string-append
			""
			"\tMY_MALLOC(8*" (number->string  env )") \n" ;extened env , in rbx
	      	;"T_"counter":\n"
	      	"\tmov rbx, rax \n" 
	      	"\tpush rbx\n"
	      	"\tmov rbx, arg_count\n"
	      	
	      	"\tshl rbx, 3\n"   ;;like multiply 8
      		"\tMY_MALLOC(rbx) \n" ; env[0] = rcx
	      	;"T2_"counter":\n"
	      	"\tpop rbx\n"
	      	"\tmov rcx, rax \n"
	
			"\tMY_MALLOC(16) \n" ;new closure rax
			;"T3_"counter":\n"
			"\tmov rdi, 0\n" ; params
			"\tmov r13,"(number->string env) "\n "
			"\tcmp r13, 0\n"
			"\tje ENV_LOOP_END_"counter "\n\n"

			"PARAM_LOOP_"counter":\n"
			"\tcmp rdi, arg_count\n" 
			"\tje PARAM_LOOP_END_"counter "\n"
			"\tmov r15, rcx\n"
			"\tpush rdi \n"
			"\tshl rdi, 3 \n"
			"\tadd r15, rdi\n"
			"\tpop rdi \n"
			"\tpush rbx\n"
			"\tmov r9, An(rdi)\n"
			"P1_"counter":\n"
			"\tmov [r15], r9\n"
			"\tpop rbx\n"
			"\tinc rdi\n"
			"\tjmp PARAM_LOOP_"counter"\n"
			"PARAM_LOOP_END_"counter ":\n\n"

			"\tmov [rbx], rcx \n" ; env[0] = rcx
			"\tmov rdi,0 \n"   ;this is i 
			"\tmov rsi, 1 \n"  ; this is j
			"\tENV_LOOP_"counter":\n"
			"\tinc rdi \n"
			"\tcmp rdi," (number->string env) "\n"
			"\tje ENV_LOOP_END_"counter"\n"
			"\tdec rdi \n"
			"\tmov rdx, env \n"
			"\tmov r8, [rdx + 8 *rdi]\n"
			"\tmov [rbx + 8 *rsi], r8\n"
			"\tinc rdi \n"
			"\tinc rsi \n"
			"\tjmp ENV_LOOP_"counter"\n"

			"ENV_LOOP_END_"counter":\n\n"

			"\tMAKE_LITERAL_CLOSURE rax, rbx, CLOSURE_CODE_"counter "\n"

			"\tjmp EXIT_"counter "\n"

			;create code closure
      		"CLOSURE_CODE_"counter":\n"
      		"\tpush rbp \n"
      		"\tmov rbp,rsp \n"
      		(code_gen body env)
      		"\tleave \n"
      		"\tret \n"
      		"\tEXIT_"counter": \n\n"
      		"\tmov rax,[rax]\n" ; insert line because in rax need to be value not address 14/2 
      		
			 	)
			 )
		)
)

 (define code_gen_applic
   (lambda (lst counter env)
 			   (let ((num_operand (number->string (length (cadr lst))))
 			   		(body_gen (reverse (map (lambda (exp) (code_gen exp env)) (cadr lst))))
 			   		(operator (code_gen (car lst) env)))
 			      (string-append
 			      	(fold-right
 			      		string-append 
 			      		""
 			      		(map (lambda (exp)
 			      			(string-append
 			      			exp "\n"
 			      		  "\tpush rax\n")) body_gen))
 			      			;"stop_applic:\n"
						  "\tmov rbx, " num_operand "\n"
		 			      "\tpush rbx\n"
		 		          "\t"operator"\n"  ;code+gen to code
		 		          "\tmov rbx, rax\n"
		 		          "\tCLOSURE_ENV(rbx) \n"
		 		          "\tpush rbx\n" ; push env to stack
		 			      "\tCLOSURE_CODE(rax)\n"
		 			      "\tcall rax\n"
		 			      "\tmov rdi, [rsp+8]\n\t"
		 			      "\tadd rdi, 2\n\t"
		 			      "\tshl rdi, 3\n\t"
		 			      "\tadd rsp,rdi\n\t"

 			       )
 			  )
 	)
) 

(define code_gen_lambda_opt
	(lambda (lst counter env)
        (let* ((num_elements (length (car lst))))
		
        (string-append  
        "\tmov rsi , arg_count \n"
        "\tmov rax , 8\n"
        "\tmul rsi\n"
        "\tmov rsi , rax\n"
        "\tMY_MALLOC (rsi) \n" 
        "\tmov rcx , rax \n"
        "\tmov rsi , arg_count \n"
        "\tmov rdx , 0 \n"
       
        "List_arg_" counter ":\n"       
        "\tcmp rdx , rsi\n"
        "\tje List_arg_END_" counter "\n"
        
        "\tmov rax , An(rdx)\n"
        "\tmov qword[rcx + 8*rdx], rax\n"
        "\tadd rdx , 1\n"
        "\tjmp List_arg_" counter  "\n"
        
        "List_arg_END_" counter ":\n"
 
        "\tmov r10 , " (number->string  env ) " \n"
        "\tadd r10 , 1\n"
        "\tmov rax , 8\n"
        
        "\tmul r10\n"
        "\tmov rsi , rax \n"
        "\tMY_MALLOC(rsi) \n"  ;malloc -> env 
        "\tmov rbx , rax \n"
                          
        "\tmov qword[rbx] , rcx\n" ;env(0) = args      
      
        "\tmov rsi , env\n"
        "\tmov rdx , 1\n"     ;rdi = counter new env
        "\tmov r8 , 0\n"      ; r8 = counter for old env
        "\tmov rax , " (number->string  env ) " \n"   
       
        "Env_loop_build_" counter":\n"
        "\tcmp rdx , rax "  "\n"
        "\tjg Env_loop_build_END" counter"\n"
        "\tmov r9 , qword[rsi + 8*r8 ] \n"  ;loop building env
        "\tmov qword[rbx + 8*rdx] , r9\n"
        
        "\tadd rdx , 1\n"
        "\tadd r8 , 1\n"
        "\tjmp Env_loop_build_"counter "\n"
        
        
        "Env_loop_build_END" counter  ":\n"
       
        "\tMY_MALLOC(16) \n"  
        "\tMAKE_LITERAL_CLOSURE rax , rbx , Lambda_body_loop" counter"\n"
        "\tjmp Lambda_body_loop_END"counter"\n" 
        
        "\tLambda_body_loop"counter ":\n"
        "\tpush rbp\n"
        "\tmov rbp , rsp\n"
        
        "\tmov rsi , " (number->string num_elements) "\n"
        "\tmov rbx , arg_count \n"
        "\tcmp rsi , rbx \n"
        "\tjne List_Params_" counter "\n" 
        "\tadd rbx , 4\n"  ;rbx= loop cond
        "\tmov rcx , 0\n"  ;rcx = counter
        "\tmov rdx , rbp\n" 
        "\tsub rdx , 8\n"  ;rdx= pointer to start
         
         "Shifting_params" counter ":\n"
         "\tcmp rcx , rbx \n"
         "\tje Shifting_params_END" counter "\n"
         "\tmov rsi ,qword [rbp+8*rcx] \n"
         "\tmov [rdx] ,rsi \n"
         "\tadd rdx , 8 \n"
         "\tadd rcx, 1 \n"
         "\tjmp Shifting_params"counter "\n"
         
        
         "Shifting_params_END"counter":\n"
         "\tmov rsi , SOB_NIL \n"
         "\tmov [rdx] , rsi \n"
         
         "\tsub rbp , 8 \n"
         "\tsub rsp , 8 \n"
         "\tmov rbx , arg_count \n"
         "\tadd rbx , 1 \n"
         "\tmov [rbp+3*8] ,rbx \n" 
         "\tjmp Body_Code_gen" counter "\n"
         
         "List_Params_" counter ":" "\n"
         "\tmov rdx ,rbp \n" 
         "\tmov rsi ," (number->string num_elements) "\n" 
         "\tmov r12 , arg_count \n"
         "\tsub r12 , rsi \n"   
         "\tmov rsi , arg_count \n"
         "\tadd rsi , 4 \n"
         "\tshl rsi , 3 \n"
         "\tadd rdx , rsi \n"  
         "\tmov r11 , 0 \n"     
         
         "\tMY_MALLOC(8)  \n"
         "\tmov rbx , rax \n"
         "\tmov rcx , SOB_NIL \n"
         "\tmov [rbx] , rcx \n"
         
         "\tMY_MALLOC(8) \n"
         "\tmov r10 , rax \n"
         "\tsub rdx , 8 \n"
         "\tmov rsi , qword[rdx] \n"
         "\tmov [r10] , rsi \n"
         "\tMY_MALLOC(16) \n"
         "\tMAKE_MALLOC_LITERAL_PAIR rax ,r10 ,rbx \n"
         "\tadd r11 , 1 \n"
         
         
         "Building_arg_list" counter ": \n"
         "\tcmp r11 , r12  \n"
         "\tje Building_arg_list_END"counter" \n"
         "\tmov r10 , rax \n"
         "\tMY_MALLOC(8) \n"
         "\tmov rbx , rax \n"
         "\tsub rdx , 8 \n"
         "\tmov rsi , qword[rdx] \n"
         "\tmov [rbx] , rsi \n"
         "\tMY_MALLOC(16) \n"
         "\tMAKE_MALLOC_LITERAL_PAIR rax ,rbx ,r10 \n"
         "\tadd r11 , 1 \n"
         "\tjmp Building_arg_list" counter "\n"
         
         "Building_arg_list_END" counter": \n"

         "\tmov rax , qword[rax] \n"
         "\tmov qword[rdx], rax \n"

         "mov r9, arg_count\n\t" 
         "add r9, 3 \n\t" 
         "shl r9, 3\n\t"
         "mov rcx , rbp \n\t"
         "add rcx, r9\n\t"

         "Remove_loop" counter ": \n" 
         "\tcmp rdx , rbp \n"
         "\tje Remove_loop_END" counter "\n"
         "\tmov rsi , qword[rdx] \n"
         "\tmov qword[rcx] ,rsi \n"
         "\tsub rcx , 8 \n"
         "\tsub rdx , 8 \n"
         "\tjmp Remove_loop" counter "\n"
        
         "Remove_loop_END" counter": \n" 
         "\tmov rsi , qword[rdx] \n"
         "\tmov qword[rcx] ,rsi \n"
         "\tmov rsp , rcx \n"
         "\tmov rbp , rcx \n"
         "\tmov rbx ," (number->string num_elements) "\n"
         "\tadd rbx , 1 \n"
         "\tmov qword[rbp + 3*8] , rbx \n" 
         
         "Body_Code_gen" counter ":\n"
          (code_gen  (caddr lst) env) 
          "\tleave\n"
          "\tret\n"   
            
        "Lambda_body_loop_END" counter ":\n"
        "\tmov rax , qword [rax]\n"   
        	)
		)
	)
)
(define code_gen_tc_applic 
  (lambda (lst counter env)
        (let*((num_args (number->string (length (cadr lst))))
              (param_reverse (reverse (map (lambda (exp) (code_gen exp env)) (cadr lst)))))
   			(string-append
 				(fold-right
 			    	string-append 
 			      		""
 			      		(map (lambda (exp) (string-append exp "\n" "push rax\n")) param_reverse))
				   "\tmov rbx, "num_args "\n"
 			       "\tpush rbx\n"
				
 		           (code_gen (car lst) env)"\n"  ;activate the code
 		          "\tmov rbx, rax\n"

 		          "\tCLOSURE_ENV(rbx) \n"
 		          "\tpush rbx \n" ; push the env in stack
 		          "\tmov r12, ret_addr \n"
 				  "\tpush r12 \n"
 			
 				  "\tmov r8, rbp \n"
 				  "\tmov rbp, old_rbp \n"
 				  "\tmov rdi,"num_args "+ 3\n"
 				  "\tmov r14, [r8 + 3 *8]\n"
 				  "\tadd r14, 4\n"
 				  
 				  "\tdec r14\n"
 				  "\tdec rdi\n"
 				  "\tmov r9, [rsp +rdi*8]\n"
 				  "\tmov [r8 + r14*8], r9\n"

 				  "Loop_Tc_applic_"counter ":\n"
 				  "\tcmp rdi , 0 \n"
 				  "\tje Loop_Tc_applic_END_"counter "\n"
 				  "\tdec r14\n"
 				  "\tdec rdi\n"
 				  "\tmov r9, [rsp +rdi*8]\n"
 				  "\tmov [r8 + r14*8], r9\n"
 				  "\tjmp Loop_Tc_applic_"counter"\n"

 				  "Loop_Tc_applic_END_"counter":\n"
 				  "\tlea rsp, [r8 + r14 *8]\n"
 				  "\tCLOSURE_CODE(rax)\n"
 				  "\tjmp rax\n"
             )
   		)
    )
)
;set disassembly-flavor intel
;layout reg