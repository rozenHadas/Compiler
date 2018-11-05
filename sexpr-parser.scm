
 (load "project/pc.scm")
 
(define <whitespace>
  (const
   (lambda (ch)
     (char<=? ch #\space))))

(define <end_of_line_comment>
   (new (*parser (char #\newline))
	      (*parser <end-of-input>)
	      (*disj 2)
	      done))

(define <line-comment>
    (new
     (*parser (char #\;))
		 (*parser <any-char>)
	   (*parser <end_of_line_comment>)
	   *diff *star

	   (*parser <end_of_line_comment>)
	   (*caten 3)
	   done))

(define <commentOfInfix>
  (new 
       (*parser (char #\#))  
       (*parser (char #\;))
       (*caten 2)
       (*pack-with (lambda(sulamit pointPsik)
                        (list->string `(,sulamit ,pointPsik))))
       (*delayed (lambda () <InfixAddOrSub>))
       (*caten 2)
       done))

(define <comment>
  (disj 
	<commentOfInfix> 
	<line-comment>))
       

(define <skip>
  (disj 
  	<whitespace> 
  	<comment>
	))


(define <commnetOfSexpr>
  (new 
    (*parser (char #\#))  
    (*parser (char #\;))
    (*caten 2)
    (*pack-with (lambda(sulamit pointPsik)
                    (list->string `(,sulamit ,pointPsik))))
    (*delayed (lambda () <sexpr>))
    (*caten 2)
   	(*parser <line-comment>)
	    (*disj 2)
       done))


(define <whitespace_or_comment_of_sexpr>
  (disj    
	<whitespace>
	<commnetOfSexpr>	))


(define <wrapped_of_comment_and_whitespace>
  (lambda (<wrapper>)
    (lambda (<p>)
      (new (*parser <wrapper>)
	   (*parser <p>)
	   (*parser <wrapper>)
	   (*caten 3)
	   (*pack-with
	    (lambda (left exp right) exp))
	   done))))

(define <main_whitespace_or_comments> 
	(<wrapped_of_comment_and_whitespace> 
	(star <whitespace_or_comment_of_sexpr>)))


(define <digits1-9>
  (range #\1 #\9))

(define <digits0-9>
  (range #\0 #\9))

(define <a-f>
  (range #\a #\f))

(define <A-F>
 (range #\A #\F))

  (define <PosNeg>
   (new  
         (*parser (char #\+))
         (*parser (char #\-))
         (*disj 2)
   done))

  (define <MulDiv>
   (new  
        (*parser (char #\*))
         (*parser (char #\/))
         (*disj 2)
    done))  

(define <PowerSymbol>
  (new
   (*parser (word "^"))
   (*parser (word "**"))
   (*disj 2)
   (*pack (lambda(x) (list->string x)))
   done))

(define <Boolean_True>
    (new
     (*parser (word-ci "#t"))
     (*pack (lambda(x) #t))
     done ))

(define <Boolean_False>
    (new
     (*parser  (word-ci "#f")) 
     (*pack (lambda(x) #f))
     done ))

(define <Boolean>
  (new
   (*parser <Boolean_True>)
   (*parser <Boolean_False>)
   (*disj 2)
   done))

(define <SymbolChar>
(new 
    (*parser <digits0-9>)
    (*parser (range #\a #\z))
    (*parser (range #\A #\Z))
    (*pack 
       (lambda (ch) (char-downcase ch)))
    (*parser (char #\!))
    (*parser (char #\$))
    (*parser (char #\^))
    (*parser (char #\*))
    (*parser (char #\-))
    (*parser (char #\_))
    (*parser (char #\=))
    (*parser (char #\+))
    (*parser (char #\<))
    (*parser (char #\>))
    (*parser (char #\?))
    (*parser (char #\/))
    (*parser (char #\\))
    (*parser (char #\"))
    (*disj 17)
    done))
  
(define <Symbol>
(new 
  (*parser <SymbolChar>) *plus
  (*pack (lambda (x) (string->symbol (list->string x)))) 
 done))
    
(define <Natural>
	(new         
		(*parser (char #\0)) *star 
		(*parser <digits1-9>)      
		(*caten 2)
		(*pack-with 
			(lambda (dig0 dig1) dig1))
		(*parser <digits0-9>) *star
		(*caten 2)
		(*pack-with 
			(lambda (x xs) 
				(string->number (list->string `(,x ,@xs)))))
		(*parser (char #\0))
		(*pack (lambda (_) 0))
		(*disj 2)

    		done))

    
(define <Integer>
  (new

       (*parser (char #\+))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (signP n) n))

       (*parser (char #\-))
       (*parser <Natural>)
       (*caten 2)
       (*pack-with
	(lambda (signM n) (- n)))

       (*parser <Natural>)

       (*disj 3)

       done))


(define <Fraction>
	(new 
		(*parser <Integer>)
		(*parser (char #\/))
		(*parser <Natural>)
		(*only-if (lambda (n) (not (zero? n))))
		(*caten 3)
		  (*pack-with
	(lambda (int div nat)
	  (/ int nat)))
       done))



(define <Number>
    (new 
        (*parser <Fraction>)
        (*parser <Integer>)
        (*disj 2)
        (*delayed (lambda () <InfixSymbolChar>))
        *not-followed-by
    done))


(define <HexChar>
  (new
  (*parser <digits0-9>)
  (*parser <A-F>)
  (*parser <a-f>)
  (*disj 3)
  done))
  

(define <CharPrefix>
  (new
   (*parser (word "#\\"))
   done))


(define <VisibleSimpleChar>
  	(range #\x21 #\xff) )

(define <NamedChar>
  (new
   (*parser (word-ci "lambda"))
   (*pack (lambda(x) #\x3bb))
   (*parser (word-ci "newline"))
   (*pack (lambda(x) #\newline))
   (*parser (word-ci "nul"))
   (*pack (lambda(x) #\nul))
   (*parser (word-ci "page"))
   (*pack (lambda(x) #\page))
   (*parser (word-ci "return"))
   (*pack (lambda(x) #\return))
   (*parser (word-ci "space"))
   (*pack (lambda(x) #\space))
   (*parser (word-ci "tab"))
   (*pack (lambda(x) #\tab))
   (*disj 7)
   done))

(define <HexUnicodeChar>
  (new
   (*parser (char-ci #\x))
   (*parser <HexChar>) *plus
   (*caten 2)
   (*pack-with
    (lambda (x hex) (integer->char (string->number (list->string hex) 16)))) 
   done))

(define <Char>
(new
    (*parser <CharPrefix>)
    (*parser <NamedChar>)
    (*parser <HexUnicodeChar>)
    (*parser <VisibleSimpleChar>)
    (*disj 3)
    (*delayed (lambda () <Symbol>)) *not-followed-by
    (*caten 2)
    (*pack-with (lambda (prefix val) val))  
    (*delayed (lambda () <skip>)) *star
    (*caten 2)
    (*pack-with (lambda (val w1)  val))
    done))



(define <StringLiteralChar>
   (new  (*parser <any-char>)
	 (*parser (char #\\))
	 (*parser (char #\"))
	 (*disj 2)
	 *diff
	 done))

(define <StringHexChar>
  (new
   (*parser (word-ci "\\x"))
   (*parser <HexChar>) *plus
   (*parser (char #\;))
   (*caten 3)
   (*pack-with (lambda (a b c) (string->number (list->string b) 16)))
   (*only-if (lambda(x)  (and (> x 0) (<  x  1114112) )))
   (*pack (lambda (hex) (integer->char hex) ))
   done))


(define <StringMetaChar>
  (new
   (*parser (word-ci "\\\\"))
   (*pack (lambda (x) #\\))
   (*parser (word-ci "\\\""))
   (*pack (lambda (x) #\"))
   (*parser (word-ci "\t"))
   (*pack (lambda(x) #\x9))
   (*parser (word-ci "\f"))
   (*pack (lambda (x) #\xc))
   (*parser (word-ci "\n"))
   (*pack (lambda (x) #\xa))
   (*parser (word-ci "\r"))
   (*pack (lambda (x) #\xd))
   (*disj 6)
   done))



(define <StringChar>
  (new
   (*parser <StringLiteralChar>)
   (*parser <StringHexChar>)
   (*parser <StringMetaChar>)

   (*disj 3)
   done))


(define <String>
  (new 
    (*parser (char #\"))
    (*parser <StringChar>) *star
    (*parser (char #\"))
    (*caten 3)
    (*pack-with
	(lambda (_ st __) (list->string st)))
   done))




(define <CBNameSyntax1>
	(new 
		(*parser  <skip>) *star
  		(*parser (char #\@))
  		(*parser  <skip>) *star
  		(*delayed (lambda () <sexpr>))
  		(*parser  <skip>) *star
  		(*caten 5)
  		(*pack-with
 		  (lambda (w1 strodel w2 sexp1 w3)
 		  		(list 'cbname sexp1)))
  	done))


(define <CBNameSyntax2>
	(new 
		(*parser  <skip>) *star
  		(*parser (char #\{))
  		(*parser  <skip>) *star
  		(*delayed (lambda () <sexpr>))
  		(*parser  <skip>) *star
  		(*parser (char #\}))
  		(*parser  <skip>) *star
  		(*caten 7)
  		(*pack-with
 		  (lambda (w1 braceOpen w2 sexp1 w3 braceClose w4)
 		  		(list 'cbname sexp1)))
  	done))

(define <CBName>
	(new 
		(*parser <CBNameSyntax1>)
		(*parser <CBNameSyntax2>)
		(*disj 2)
		done))



(define <ProperList>
(new
    (*delayed (lambda () <skip>)) *star
    (*parser (char #\())
    (*delayed (lambda () <skip>)) *star
    (*delayed (lambda () <sexpr>)) *star
    (*delayed (lambda () <skip>)) *star
    (*parser (char #\)))
    (*caten 6)
    (*pack-with (lambda (w1 braceOpen w2 sexp w3 braceClose) sexp))
     done))


(define <ImproperList>

(new
    (*delayed (lambda () <skip>)) *star
    (*parser (char #\())
    (*delayed (lambda () <skip>)) *star
    (*delayed (lambda () <sexpr>)) *plus
    (*delayed (lambda () <skip>)) *star
    (*parser (char #\.))
    (*delayed (lambda () <skip>)) *star
    (*delayed (lambda () <sexpr>))
    (*delayed (lambda () <skip>)) *star
    (*parser (char #\)))
    (*caten 10)
    (*pack-with (lambda (w1 braceOpen w2 sexpr1 w3 point w4 sexpr2 w5 braceClose) (append sexpr1 sexpr2)))
     done))


(define <Vector>
(new (*delayed (lambda () <skip>)) *star
     (*parser (char #\#))
     (*parser (char #\())
     (*delayed (lambda () <skip>)) *star
     (*delayed (lambda () <sexpr>)) *star
     (*delayed (lambda () <skip>)) *star
     (*parser (char #\)))
     (*caten 7)
     (*pack-with (lambda (w1 sulamit braceOpen w2 sexpr w3 braceClose) (list->vector sexpr)))
     done))



 (define <Quoted>
  	(new 
  		(*parser (char #\'))
  		(*delayed (lambda () <sexpr>))
  		(*caten 2)
  		(*pack-with
 		  (lambda (q sexp1) (list 'quote sexp1)))
  	done))
 


 (define <QuasiQuoted>
  	(new 
  		 (*parser (char #\`))
  		 (*delayed (lambda () <sexpr>))
  		 (*caten 2)
  		 (*pack-with
 		  (lambda (_ q) (list 'quasiquote q)))
  	done))

 
 (define <Unquoted>
  	(new 
  		(*parser (char #\,))      
        (*parser (char #\@)) *not-followed-by    
  		(*delayed (lambda () <sexpr>))
        (*caten 2)       
  		(*pack-with
 		  (lambda (_ q) (list 'unquote q)))
  	done))

 
 (define <UnquoteAndSpliced>
  	(new
  		(*parser (char #\,))
                           
  		(*parser (char #\@))
  		(*delayed (lambda () <sexpr>))
           (*caten 3)
  		 (*pack-with
 		  (lambda (_ __ qs) (list 'unquote-splicing  qs)))
  	done))



(define <InfixPrefixExtensionPrefix>
    (new 
        (*parser (word-ci "##"))
        (*parser (word-ci "#%"))
        (*disj 2)
        (*pack 
        (lambda (x)
        (list->string x)))
    done))


(define <InfixSymbolChar>
	(new 
		 (*parser  <digits0-9>)
		 (*parser (range #\a #\z))
		 (*parser (range #\A #\Z))
		 (*pack (lambda (ch) (char-downcase ch)))   
		 
		 (*parser 	(char #\!))
		 (*parser 	(char #\$))
		 (*parser 	(char #\_))
		 (*parser 	(char #\=))
		 (*parser 	(char #\<))
		 (*parser 	(char #\>))
		 (*parser 	(char #\?))

	(*disj 10)
	done))


(define <InfixSymbol>                                 
	(new (*parser <InfixSymbolChar>)*plus
    	 (*pack 
    	 (lambda (ch) 
    	 (string->symbol (list->string ch))))
    	 done))

(define <InfixAddOrSub>
  (new
   (*delayed (lambda ()  <skip>)) *star
   (*delayed (lambda () <InfixMulOrDiv>))
   (*delayed (lambda ()  <skip>)) *star
   (*parser (char #\+))
   (*parser (char #\-))
   (*disj 2)
   (*delayed (lambda ()  <skip>)) *star
   (*delayed (lambda () <InfixMulOrDiv>))
   (*delayed (lambda ()  <skip>)) *star
   (*caten 5)
   (*pack-with (lambda (w2 op w3 eSec w4) (cons op eSec))) *star
   (*caten 3)
   (*pack-with (lambda (w1 eFirst lst) 
    			(fold-left (lambda (a b) `(,(string->symbol (string (car b))) , a ,(cdr b))) eFirst lst)))                                             
 done))


(define <InfixMulOrDiv>
  (new
   (*delayed (lambda ()  <skip>)) *star
   (*delayed (lambda () <InfixPow>))
   (*delayed (lambda ()  <skip>)) *star
   (*parser (char #\/))
   (*parser (char #\*))
   (*disj 2)
   (*delayed (lambda ()  <skip>)) *star
   (*delayed (lambda () <InfixPow>))
   (*delayed (lambda ()  <skip>)) *star
   (*caten 5)
   (*pack-with (lambda (w2 op w3 eSec w4) (begin (cons op eSec)))) 
    *star
   (*caten 3)                              
   (*pack-with (lambda (W1 eFirst lst) 
   				(begin (fold-left (lambda (a b) `(,(string->symbol (string (car b))) ,a ,(cdr b))) eFirst lst))))
      
    done))



(define <InfixPow>
(new (*delayed (lambda ()  <skip>)) *star
     (*delayed (lambda () <InfixArrayGet>))
     (*delayed (lambda ()  <skip>)) *star
     (*parser  <PowerSymbol>)
     (*delayed (lambda ()  <skip>)) *star
     (*delayed (lambda () <InfixArrayGet>))
     (*delayed (lambda ()  <skip>)) *star
     (*caten 5)
     (*pack-with (lambda (w2 operator w3 element2 w4)  (begin  (cons operator element2)))) *star
     (*caten 3)
     (*pack-with (lambda (w1 element1 pow_el2)  (begin (fold-left (lambda (a b) `(expt  ,(cdr b),a))  (if (not (null?  pow_el2))  (cdr (car (reverse pow_el2))) element1) (cdr (reverse (cons (cons #\^ element1) pow_el2)))))))
     done))  
          


 (define <SymbolNotOperator>
    (new 
     (*parser <SymbolChar>)
     (*only-if (lambda (x) (and (not (equal? x #\+)) (not (equal? x #\-)) (not (equal? x #\/)) (not (equal? x #\*)) (not (equal? x #\^))) ))
      *plus     
     (*pack (lambda (x) (string->symbol (list->string x))))                            
  done))


 (define <notOperator>
    (new 
         (*parser <Number>)
         (*parser <SymbolNotOperator>) 
         (*only-if (lambda (x) (not (null? (string->list (symbol->string x))))))
         (*caten 2) 
         (*pack-with (lambda (x y) (string->symbol (list->string (append (string->list (number->string x)) (string->list (symbol->string y))))))) 
         (*parser <Number>)
         (*parser <SymbolNotOperator>)
         (*disj 3)
         (*delayed (lambda () <InfixSexprEscape>))
         (*disj 2)
         done))

 
(define <InfixParen>
(new   
       (*delayed (lambda ()  <skip>)) *star
       (*parser (char #\())
       (*delayed (lambda ()  <skip>)) *star
       (*delayed (lambda () <InfixAddOrSub>))
       (*delayed (lambda ()  <skip>)) *star
       (*parser (char #\)))
       (*caten 6)
       (*pack-with (lambda (w1 openBracket w2 addSubExp w3 closeBracket) addSubExp))
         (*delayed (lambda () <notOperator>))   
          
       (*disj 2)

    done))

(define <InfixNeg>
 (new  
      (*delayed (lambda ()  <skip>)) *star
      (*parser (char #\-))
      (*delayed (lambda ()  <skip>)) *star
      (*parser (char #\-))                        
      (*delayed (lambda() <InfixArrayGet>))
      (*disj 2)                        
      (*only-if (lambda (x) (not (equal? x #\-))))
      (*parser <InfixParen>)
      (*disj 2)
      (*caten 4)
      (*pack-with (lambda (x z w y) (if (number? y) (if (null? w) (- y) `(- ,y)) `(- ,y))))

      (*parser <InfixParen>)

      (*disj 2)  
      
      done)) 

 (define <InfixArgList>
 (new  
 	(*delayed (lambda ()  <skip>)) *star
    (*delayed (lambda () <InfixAddOrSub>))  
    (*pack (lambda(x) (list x))) 
    (*delayed (lambda ()  <skip>)) *star
    (*parser (char #\,))
    (*delayed (lambda ()  <skip>)) *star
    (*delayed (lambda () <InfixAddOrSub>)) 
    (*delayed (lambda ()  <skip>)) *star
	(*caten 4)  
	(*pack-with (lambda (psik w3 inf1 w4)  (list inf1))) *star
    (*caten 4)
	(*pack-with (lambda(s0 inf1 v inf2) (fold-left (lambda(x y) (append x y)) inf1  inf2)))
    
 done))


 
 (define <InfixFuncall>
 (new  
    (*delayed (lambda ()  <skip>)) *star
    (*delayed (lambda () <InfixNeg>))   
    (*delayed (lambda ()  <skip>)) *star
    (*parser (char #\())
    (*delayed (lambda ()  <skip>)) *star
    (*delayed (lambda () <InfixArgList>))
    (*parser <epsilon>)
    (*disj 2)  
    (*delayed (lambda ()  <skip>)) *star
    (*parser (char #\)))
    (*caten 6)
    (*pack-with (lambda (w1 braceOpen w2 argOrEpsilion w3 braceClose) argOrEpsilion)) *plus
    (*caten 3)    
    (*pack-with (lambda(w1 neg  infArgOrEpsilion) (fold-left (lambda(x y) `( ,x ,@y)) neg  infArgOrEpsilion)))
    (*delayed (lambda () <InfixNeg>)) 
    (*disj 2)
    done))

 
(define <FuncallAndArrget>
   (new  (*delayed (lambda() <InfixFuncall>))
         (*delayed (lambda ()  <skip>)) *star
         (*parser (char #\[))
         (*delayed (lambda ()  <skip>)) *star           
         (*delayed (lambda() <InfixAddOrSub>))
         (*delayed (lambda ()  <skip>)) *star         
         (*parser (char #\]))
         (*caten 5)
         (*pack-with (lambda(braceOpen w1 addsub w2 braceClose) addsub)) *star
         (*parser (char #\())
         (*delayed (lambda ()  <skip>)) *star
         (*delayed (lambda() <InfixArgList>))
         (*parser <epsilon>)
         (*disj 2)
         (*delayed (lambda ()  <skip>)) *star
         (*parser (char #\)))
         (*caten 5)
         (*pack-with (lambda (braceOpen w1 ArgOrEpsilon w2 braceClose) ArgOrEpsilon)) *star
         (*parser (char #\[))
         (*delayed (lambda ()  <skip>)) *star           
         (*delayed (lambda() <InfixAddOrSub>))
         (*delayed (lambda ()  <skip>)) *star         
         (*parser (char #\]))
         (*caten 5)
         (*pack-with (lambda(braceOpen w1 addsub2 w2 braceClose) addsub2)) *star
         (*parser (char #\())
         (*delayed (lambda ()  <skip>)) *star
         (*delayed (lambda() <InfixArgList>))
         (*parser <epsilon>)
         (*disj 2)
         (*delayed (lambda ()  <skip>)) *star
         (*parser (char #\)))
         (*caten 5)
         (*pack-with (lambda (braceOpen w1 ArgOrEpsilon2 w2 braceClose) ArgOrEpsilon2)) *star               
         (*caten 6)
         (*pack-with 
         (lambda (infuncall w1 addsub ArgOrEpsilon addsub2 ArgOrEpsilon2) 
        (fold-left (lambda (a b) `(,a ,@b))
        (fold-left (lambda (a b) `(vector-ref ,a,b))  
        (fold-left (lambda (a b) `(,a ,@b)) 
        (fold-left (lambda (a b) `(vector-ref ,a,b)) infuncall addsub) ArgOrEpsilon) addsub2) ArgOrEpsilon2))) 
         done))


 


    

(define <InfixArrayGet>
    (new 
      (*delayed (lambda ()  <skip>)) *star
      (*parser (char #\-))
      (*delayed (lambda ()  <skip>)) *star
      (*delayed (lambda () <Number>))
      (*caten 4)
      (*pack-with (lambda (w1 minus w2 num)  (- num)))
      
      (*delayed (lambda ()  <skip>)) *star
      (*parser (char #\[))
       (*delayed (lambda () <whitespace_or_comment_of_sexpr>)) *star
      (*delayed (lambda () <InfixAddOrSub>))
       (*delayed (lambda () <whitespace_or_comment_of_sexpr>)) *star
      (*parser (char #\]))
      (*caten 6)
      (*pack-with (lambda (w1 openbracket w2 inf1 w3 closebracket)  inf1))   *plus
      (*caten 2)
      (*pack-with (lambda (num inf1) (fold-left (lambda (x y) `(vector-ref ,x ,y)) num  inf1)))
      (*delayed (lambda () <FuncallAndArrget>))          
      (*delayed (lambda ()  <skip>)) *star
      (*delayed (lambda () <InfixFuncall>))
      (*delayed (lambda ()  <skip>)) *star
      (*parser (char #\[))
       (*delayed (lambda () <whitespace_or_comment_of_sexpr>)) *star
      (*delayed (lambda () <InfixAddOrSub>))
       (*delayed (lambda () <whitespace_or_comment_of_sexpr>)) *star
      (*parser (char #\]))                 
      (*caten 6)
      (*pack-with (lambda (w1 openbracket w2 inf1 w3 closebracket)  inf1))   *star
      (*caten 3)
      (*pack-with (lambda (w1 inffunc infaddsub) (fold-left (lambda (x y) `(vector-ref ,x ,y)) inffunc  infaddsub)))       
      (*disj 3)                  
      done))


 (define <InfixSexprEscape>
 (new (*delayed (lambda ()  <skip>)) *star 
     (*parser <InfixPrefixExtensionPrefix>)
     (*delayed (lambda ()  <skip>)) *star
     (*delayed (lambda ()<sexpr>))
     (*caten 4)
     (*pack-with (lambda (w1 symInfix w2 sexp) sexp))
     done)) 


(define <InfixExpression>
(new (*parser <InfixAddOrSub>)
      done))
  
(define <InfixExtension>

(new 
     (*parser <InfixPrefixExtensionPrefix>)
     (*parser  <skip>) *star
     (*parser <InfixExpression>)
     (*caten 3)
     (*pack-with (lambda (sym _ expr) expr))

     done))

 
     


      


(define <sexpr>
   (<main_whitespace_or_comments>
(new
   (*parser <Boolean>) 
   (*parser <Number>)
   (*parser <Symbol>)
   (*parser (range #\0 #\9))
   *diff
   *not-followed-by
   (*parser <Char>)
   (*parser <String>)
   (*parser <Symbol>)
   (*parser <ProperList>)
   (*parser <ImproperList>)
   (*parser <Vector>)
   (*parser <Quoted>)
   (*parser <QuasiQuoted>)
   (*parser <Unquoted>)
   (*parser <UnquoteAndSpliced>)
   (*parser <CBName>)
   (*parser <InfixExtension>)
   (*disj 14)
        
   done)))
