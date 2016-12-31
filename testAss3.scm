;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; Comp171 - ASS3 - Tests

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "compiler.scm")
(define test-func (lambda (x) 
		    (annotate-tc
		      (pe->lex-pe
			(box-set
			  (remove-applic-lambda-nil
			    (eliminate-nested-defines (parse x))))))))

; Aliases for convenience 			  
(define c cons)
(define l list)

(define tests-counter 1)

(define show-difference
  (lambda (actual expected)
    (if (or (null? actual) (null? expected)) ""
      (if (equal? (car actual) (car expected))
	  (begin (display (format "\033[1;32m~s\033[0m" (car actual)))
			(show-difference (cdr actual) (cdr expected)))
	  (begin (display (format "\033[1;31m~s\033[0m" (car actual)))
			(show-difference (cdr actual) (cdr expected)))))
))	
			    
(define assert
	(lambda (input expected-output)
		(display (format "~s) ~s\n" tests-counter input))
		(set! tests-counter (+ 1 tests-counter))
		(let* ((actual-output (test-func input)))
			(cond ((equal? actual-output expected-output)
				(display (format "\033[1;32m Success! ☺ \033[0m \n\n")) #t)
				(else 
				(display (format "\033[1;31mFailed! ☹\033[0m\n\n\033[1;34mExpected:\n ~s\033[0m\n\n\033[1;29mActual:\n ~s\033[0m\n\n" expected-output actual-output))
				#f))
			)))
			
(define runTests
  (lambda (tests-name lst)
	(newline)
	(display tests-name)
	(display ":")
	(newline)
	(display "==============================================")
	(newline)
	(let ((results (map (lambda (x) (assert (car x) (cdr x))) lst)))
	(newline)
	(cond ((andmap (lambda (exp) (equal? exp #t)) results)	
		(display (format "\033[1;32m~s Tests: SUCCESS! ☺ \033[0m\n \n" tests-name)) #t)		
		(else
		(display (format "\033[1;31m~s Tests: FAILED! ☹ \033[0m\n \n" tests-name)) #f)))
))

(define runAllTests
  (lambda (lst)
    (let ((results (map (lambda (test) (runTests (car test) (cdr test))) lst)))
      	(cond ((andmap (lambda (exp) (equal? exp #t)) results)		
		(display "\033[1;32m !!!!!  ☺  ALL TESTS SUCCEEDED  ☺  !!!!\033[0m\n") #t)
		(else (display "\033[1;31m #####  ☹  SOME TESTS FAILED  ☹  #####\033[0m\n") #f))
		(newline))
))

(define Tests
  (l
    (c '(lambda (x) x) '(lambda-simple (x) (pvar x 0)))
    (c '(lambda (x y) x y (lambda (a b c) a b c))
    '(lambda-simple (x y) (seq ((pvar x 0) (pvar y 1) (lambda-simple
	(a b c) (seq ((pvar a 0) (pvar b 1) (pvar c 2))))))))

    (c '(lambda (x y) (lambda (a b c) a b c) x y)
	'(lambda-simple (x y) (seq ((lambda-simple (a b c) (seq ((pvar a 0) (pvar b 1) (pvar c 2))))
				    (pvar x 0) (pvar y 1)))))
				    
    (c '(lambda (x y) ((lambda (a b c) a b c) 5 6 7) x y)
	'(lambda-simple (x y) (seq ((applic (lambda-simple (a b c)
			      (seq ((pvar a 0) (pvar b 1) (pvar c 2))))
				   ((const 5) (const 6) (const 7)))
				   (pvar x 0) (pvar y 1)))))
				   
    (c '(lambda (a b c d e) ((lambda (a b c) (lambda (z e1) (e1 5))) 5 6 7))
	'(lambda-simple (a b c d e) (tc-applic (lambda-simple (a b c)
				    (lambda-simple (z e1) (tc-applic (pvar e1 1) ((const 5)))))
							  ((const 5) (const 6) (const 7)))))
							  
    (c '(lambda (a b c d e) ((lambda (a b c) (lambda () 5))))
       '(lambda-simple
	(a b c d e)
	(tc-applic
	(lambda-simple (a b c) (lambda-simple () (const 5)))
	())))

    (c '(lambda (x) (define a 5) b)
       '(lambda-simple
	(x)
	(tc-applic
	  (lambda-simple
	    (a)
	    (seq ((set (pvar a 0) (const 5)) (fvar b))))
	  ((const #f)))))

    (c '(lambda (x) (define a 5) (define b 6) b1 b2 b3)
       '(lambda-simple
	  (x)
	  (tc-applic
	    (lambda-simple
	      (a b)
	      (seq ((set (pvar a 0) (const 5))
		    (set (pvar b 1) (const 6))
		    (fvar b1)
		    (fvar b2)
		    (fvar b3))))
	    ((const #f) (const #f)))))
	    
     (c '(lambda (x) (lambda (y) (define x 10) (a 5)))
	'(lambda-simple
	  (x)
	  (lambda-simple
	    (y)
	    (tc-applic
	      (lambda-simple
		(x)
		(seq ((set (pvar x 0) (const 10))
		      (tc-applic (fvar a) ((const 5))))))
	      ((const #f))))))

      (c '(lambda (x) (define y (lambda () (define a 5) 4)) 1)
         '(lambda-simple
	    (x)
	    (tc-applic
	      (lambda-simple
		(y)
		(seq ((set (pvar y 0)
			  (lambda-simple
			    ()
			    (tc-applic
			      (lambda-simple
				(a)
				(seq ((set (pvar a 0) (const 5)) (const 4))))
			      ((const #f)))))
		      (const 1))))
	      ((const #f)))))

	      
      ;'(lambda (x) (define a 5) (lambda (y) (define x 10) (a 5)))
      ;'(lambda (x) (define a 5) (define a 123) (lambda (y) (define x 10) (a 5)))
     
      (c '(lambda (z) (define a 5) (define b 123) (lambda (y) (define x 10) 
	  (define x1 (lambda (abc) (define a 56) (define x1 10) (+ 1 2))) (f 32 45 'a)) (a 5))
	  '(lambda-simple
	    (z)
	    (tc-applic
	      (lambda-simple
		(a b)
		(seq ((set (pvar a 0) (const 5))
		      (set (pvar b 1) (const 123))
		      (lambda-simple
			(y)
			(tc-applic
			  (lambda-simple
			    (x x1)
			    (seq ((set (pvar x 0) (const 10))
				    (set (pvar x1 1)
					(lambda-simple
					  (abc)
					  (tc-applic
					    (lambda-simple
					      (a x1)
					      (seq ((set (pvar a 0) (const 56))
						      (set (pvar x1 1) (const 10))
						      (tc-applic
							(fvar +)
							((const 1) (const 2))))))
					    ((const #f) (const #f)))))
				    (tc-applic
				      (fvar f)
				      ((const 32) (const 45) (const a))))))
			  ((const #f) (const #f))))
		      (tc-applic (pvar a 0) ((const 5))))))
	      ((const #f) (const #f)))))
     
     (c '(define x (lambda (x) x))
        '(def (fvar x) (lambda-simple (x) (pvar x 0))))
        
     (c '(define my-even? (lambda (e) (define even? (lambda (n) (or (zero? n) (odd? (- n 1))))) #t))
	'(def (fvar my-even?)
	  (lambda-simple
	    (e)
	    (tc-applic
	      (lambda-simple
		(even?)
		(seq ((set (pvar even? 0)
			    (lambda-simple
			      (n)
			      (or ((applic (fvar zero?) ((pvar n 0)))
				    (tc-applic
				      (fvar odd?)
				      ((applic
					(fvar -)
					((pvar n 0) (const 1)))))))))
			(const #t))))
	      ((const #f))))))

      (c '(define odd? (lambda (n) (and (positive? n) (even? (- n 1)))))
	  '(def (fvar odd?)
	    (lambda-simple
	      (n)
	      (if3 (applic (fvar positive?) ((pvar n 0)))
		    (tc-applic
		      (fvar even?)
		      ((applic (fvar -) ((pvar n 0) (const 1)))))
		    (const #f)))))

      (c '(even? e)
	 '(applic (fvar even?) ((fvar e))))
      
      (c '(lambda x (lambda (a . b) a (lambda (b) b c (f (lambda (y) (define a 5) a)))))
	 '(lambda-var
	  x
	  (lambda-opt
	    (a)
	    b
	    (seq ((pvar a 0)
		  (lambda-simple
		    (b)
		    (seq ((pvar b 0)
			    (fvar c)
			    (tc-applic
			      (fvar f)
			      ((lambda-simple
				(y)
				(tc-applic
				  (lambda-simple
				    (a)
				    (seq ((set (pvar a 0) (const 5)) (pvar a 0))))
				  ((const #f))))))))))))))


     (c '(lambda (x) ((f (lambda (z) z)) (lambda (y) y)))
	'(lambda-simple
	  (x)
	  (tc-applic
	    (applic (fvar f) ((lambda-simple (z) (pvar z 0))))
	    ((lambda-simple (y) (pvar y 0))))))

      (c '(lambda (x) (lambda (y) (lambda (z) z)))
         '(lambda-simple
	    (x)
	    (lambda-simple (y) (lambda-simple (z) (pvar z 0)))))

      (c '(lambda (x) (lambda (y) (lambda (z) (define x 1) (define y 2) (define z 3) y)))
         '(lambda-simple
	    (x)
	    (lambda-simple
	      (y)
	      (lambda-simple
		(z)
		(tc-applic
		  (lambda-simple
		    (x y z)
		    (seq ((set (pvar x 0) (const 1))
			  (set (pvar y 1) (const 2))
			  (set (pvar z 2) (const 3))
			  (pvar y 1))))
		  ((const #f) (const #f) (const #f)))))))
       
      (c '(lambda (x . y) 1)
	 '(lambda-opt (x) y (const 1)))
	 
      (c '(lambda (x . y) (lambda (x) y x (set! x 1) (* x 1)))
	 '(lambda-opt
	    (x)
	    y
	    (lambda-simple
	      (x)
	      (seq ((bvar y 0 1)
		    (pvar x 0)
		    (set (pvar x 0) (const 1))
		    (tc-applic (fvar *) ((pvar x 0) (const 1))))))))

      (c '(lambda (x) x (f (lambda (y) (define a 5) y)))
	 '(lambda-simple
	    (x)
	    (seq ((pvar x 0)
		  (tc-applic
		    (fvar f)
		    ((lambda-simple
			(y)
			(tc-applic
			  (lambda-simple
			    (a)
			    (seq ((set (pvar a 0) (const 5)) (bvar y 0 0))))
			  ((const #f))))))))))
			  
    (c '(let ((a 0))
	  (list
	  (lambda () a)
	  (lambda () (set! a (+ a 1)))
	  (lambda (b) (set! a b))))	
       '(applic
	  (lambda-simple
	    (a)
	    (seq ((set (pvar a 0) (box (pvar a 0)))
		  (tc-applic
		    (fvar list)
		    ((lambda-simple () (box-get (bvar a 0 0)))
		      (lambda-simple
			()
			(box-set
			  (bvar a 0 0)
			  (applic (fvar +) ((box-get (bvar a 0 0)) (const 1)))))
		      (lambda-simple (b) (box-set (bvar a 0 0) (pvar b 0))))))))
	  ((const 0))))
	  
    (c '(let* ((c 0)
	  (a (box c)))
	  (list
	  (lambda () a)
	  (lambda () (set! a (+ a 1)))
	  (lambda (b) (set! a b))))
	'(applic
	  (lambda-simple
	    (c)
	    (tc-applic
	      (lambda-simple
		(a)
		(seq ((set (pvar a 0) (box (pvar a 0)))
		      (tc-applic
			(fvar list)
			((lambda-simple () (box-get (bvar a 0 0)))
			  (lambda-simple
			    ()
			    (box-set
			      (bvar a 0 0)
			      (applic
				(fvar +)
				((box-get (bvar a 0 0)) (const 1)))))
			  (lambda-simple
			    (b)
			    (box-set (bvar a 0 0) (pvar b 0))))))))
	      ((applic (fvar box) ((pvar c 0))))))
	  ((const 0))))

    (c '(lambda (a . b) a)
       '(lambda-opt (a) b (pvar a 0)))
       
    (c '(lambda (a . b) (lambda () a))
       '(lambda-opt (a) b (lambda-simple () (bvar a 0 0))))
       
    (c '(lambda (a . b) (begin (lambda () a) (set! a 5)))
       '(lambda-opt
	  (a)
	  b
	  (seq ((set (pvar a 0) (box (pvar a 0)))
		(lambda-simple () (box-get (bvar a 0 0)))
		(box-set (pvar a 0) (const 5))))))
		
    (c '(let ((a 0) (c 1))
	  (list
	  (lambda () a)
	  (lambda () (set! a (+ a 1)))
	  (lambda (b) (set! a b))
	  (lambda () c)
	  (lambda () (set! c (+ a 1)))
	  (lambda (b) (set! c b))    
	))
	'(applic
	  (lambda-simple
	    (a c)
	    (seq ((set (pvar a 0) (box (pvar a 0)))
		  (set (pvar c 1) (box (pvar c 1)))
		  (tc-applic
		    (fvar list)
		    ((lambda-simple () (box-get (bvar a 0 0)))
		      (lambda-simple
			()
			(box-set
			  (bvar a 0 0)
			  (applic (fvar +) ((box-get (bvar a 0 0)) (const 1)))))
		      (lambda-simple (b) (box-set (bvar a 0 0) (pvar b 0)))
		      (lambda-simple () (box-get (bvar c 0 1)))
		      (lambda-simple
			()
			(box-set
			  (bvar c 0 1)
			  (applic (fvar +) ((box-get (bvar a 0 0)) (const 1)))))
		      (lambda-simple (b) (box-set (bvar c 0 1) (pvar b 0))))))))
	  ((const 0) (const 1))))
	  
    (c '(lambda (a b) (begin a (set! a b) (lambda () a) (lambda (a b) (+ a b))))
       '(lambda-simple
	  (a b)
	  (seq ((set (pvar a 0) (box (pvar a 0)))
		(box-get (pvar a 0))
		(box-set (pvar a 0) (pvar b 1))
		(lambda-simple () (box-get (bvar a 0 0)))
		(lambda-simple
		  (a b)
		  (tc-applic (fvar +) ((pvar a 0) (pvar b 1))))))))
		  
    (c '(lambda (a) a)
       '(lambda-simple (a) (pvar a 0)))
       
    (c '(lambda (a b) a c (lambda (a . b) b))
       '(lambda-simple
	  (a b)
	  (seq ((pvar a 0) (fvar c) (lambda-opt (a) b (pvar b 1))))))

    (c '(lambda (a b) a (lambda (b . c) b))
       '(lambda-simple
	  (a b)
	  (seq ((pvar a 0) (lambda-opt (b) c (pvar b 0))))))

    (c '(lambda () (+ 1 2))
       '(lambda-simple
	  ()
	  (tc-applic (fvar +) ((const 1) (const 2)))))

    (c '(lambda (+) (+ 1 2))
       '(lambda-simple
	  (+)
	  (tc-applic (pvar + 0) ((const 1) (const 2)))))

    (c '(lambda a a)
       '(lambda-var a (pvar a 0)))
       
    (c '(a (lambda a a) (lambda (a) a) a (lambda (a) (lambda () a) a))
       '(applic
	  (fvar a)
	  ((lambda-var a (pvar a 0))
	    (lambda-simple (a) (pvar a 0))
	    (fvar a)
	    (lambda-simple
	      (a)
	      (seq ((lambda-simple () (bvar a 0 0)) (pvar a 0)))))))

    (c '((lambda (a) a) (lambda (b) b d) (lambda (c) c))
       '(applic
	  (lambda-simple (a) (pvar a 0))
	  ((lambda-simple (b) (seq ((pvar b 0) (fvar d))))
	    (lambda-simple (c) (pvar c 0)))))

    (c '((lambda (a) a) (lambda (b) b) (lambda (c) c) d)
       '(applic
	  (lambda-simple (a) (pvar a 0))
	  ((lambda-simple (b) (pvar b 0))
	    (lambda-simple (c) (pvar c 0))
	    (fvar d))))
	    
    (c '(lambda (a b) (lambda (c) a))
       '(lambda-simple (a b) (lambda-simple (c) (bvar a 0 0))))
       
    (c '(lambda (a b) (lambda (c) a (lambda () b)))
       '(lambda-simple
	  (a b)
	  (lambda-simple
	    (c)
	    (seq ((bvar a 0 0) (lambda-simple () (bvar b 1 1)))))))

    (c '(lambda () a (lambda () (lambda () b)))
       '(lambda-simple
	  ()
	  (seq ((fvar a)
		(lambda-simple () (lambda-simple () (fvar b)))))))
    
    (c '(f (g (h (a (b (abc (lambda () a (lambda () (lambda () b)))))))))
       '(applic
	  (fvar f)
	  ((applic
	    (fvar g)
	    ((applic
		(fvar h)
		((applic
		  (fvar a)
		  ((applic
		      (fvar b)
		      ((applic
			(fvar abc)
			((lambda-simple
			    ()
			    (seq ((fvar a)
				  (lambda-simple
				    ()
				    (lambda-simple () (fvar b)))))))))))))))))))

    (c '(lambda (a b) (lambda (c) a (lambda () b (lambda (d) c))))
       '(lambda-simple
	  (a b)
	  (lambda-simple
	    (c)
	    (seq ((bvar a 0 0)
		  (lambda-simple
		    ()
		    (seq ((bvar b 1 1) (lambda-simple (d) (bvar c 1 0))))))))))

    (c '(f (lambda (a b) (lambda (c) a (lambda () (lambda (d) (lambda x a))))))
       '(applic
	  (fvar f)
	  ((lambda-simple
	    (a b)
	    (lambda-simple
	      (c)
	      (seq ((bvar a 0 0)
		      (lambda-simple
			()
			(lambda-simple (d) (lambda-var x (bvar a 3 0)))))))))))

    (c '((lambda (a) (begin a (lambda () a) (lambda (a) a))))
       '(applic
	  (lambda-simple
	    (a)
	    (seq ((pvar a 0)
		  (lambda-simple () (bvar a 0 0))
		  (lambda-simple (a) (pvar a 0)))))
	  ()))

    (c '(lambda (a b) c d (lambda () c d))
       '(lambda-simple
	  (a b)
	  (seq ((fvar c)
		(fvar d)
		(lambda-simple () (seq ((fvar c) (fvar d))))))))

    (c '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))
       '(applic
	  (fvar x)
	  ((lambda-simple
	    (x)
	    (tc-applic
	      (pvar x 0)
	      ((lambda-simple
		  ()
		  (tc-applic
		    (bvar x 0 0)
		    ((lambda-simple
		      ()
		      (tc-applic (bvar x 1 0) ((bvar x 1 0)))))))))))))
		      
    (c '(lambda (a b) (lambda (c) (+ a b c)))
       '(lambda-simple
	  (a b)
	  (lambda-simple
	    (c)
	    (tc-applic
	      (fvar +)
	      ((bvar a 0 0) (bvar b 0 1) (pvar c 0))))))

    (c '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1)))))) 
       '(def (fvar fact)
	  (lambda-simple
	    (n)
	    (if3 (applic (fvar zero?) ((pvar n 0)))
		  (const 1)
		  (tc-applic
		    (fvar *)
		    ((pvar n 0)
		      (applic
			(fvar fact)
			((applic (fvar -) ((pvar n 0) (const 1)))))))))))

   (c '(set! x (f 1))
      '(set (fvar x) (applic (fvar f) ((const 1)))))
      
    (c '(or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))
       '(or ((applic (fvar f) ((const 1))) (applic (fvar f) ((applic (fvar f) ((const 1)))))
	  (applic
	    (fvar a)
	    ((applic
	      (fvar a)
	      ((applic
		  (fvar b)
		  ((applic (fvar c) ((const 2))) (fvar d)))))))
	  (applic (fvar g) ((const 2))) (applic (fvar z) ()) (const 1)
	  (applic (fvar h) ((const 3)))
	  (applic
	    (fvar a)
	    ((applic
	      (fvar a)
	      ((applic
		  (fvar b)
		  ((applic (fvar c) ((const 2))) (fvar d))))))))))
		  
    (c '(lambda (a) (or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))))		    
       '(lambda-simple
	  (a)
	  (or ((applic (fvar f) ((const 1))) (applic (fvar f) ((applic (fvar f) ((const 1)))))
		(applic
		  (pvar a 0)
		  ((applic
		    (pvar a 0)
		    ((applic
			(fvar b)
			((applic (fvar c) ((const 2))) (fvar d)))))))
		(applic (fvar g) ((const 2))) (applic (fvar z) ()) (const 1)
		(applic (fvar h) ((const 3)))
		(tc-applic
		  (pvar a 0)
		  ((applic
		    (pvar a 0)
		    ((applic
			(fvar b)
			((applic (fvar c) ((const 2))) (fvar d)))))))))))
			
    (c '(lambda (a) (or (f 1) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))) (a (b 1)))
       '(lambda-simple
	  (a)
	  (seq ((or ((applic (fvar f) ((const 1))) (applic (fvar f) ((applic (fvar f) ((const 1)))))
		      (applic
			(pvar a 0)
			((applic
			  (pvar a 0)
			  ((applic
			      (fvar b)
			      ((applic (fvar c) ((const 2))) (fvar d)))))))
		      (applic (fvar g) ((const 2))) (applic (fvar z) ()) (const 1)
		      (applic (fvar h) ((const 3)))
		      (applic
			(pvar a 0)
			((applic
			  (pvar a 0)
			  ((applic
			      (fvar b)
			      ((applic (fvar c) ((const 2))) (fvar d)))))))))
		(tc-applic (pvar a 0) ((applic (fvar b) ((const 1)))))))))

    (c '(and (a (a (b (c 2) d))) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))
       '(if3 (applic
	  (fvar a)
	  ((applic
	      (fvar a)
	      ((applic
		(fvar b)
		((applic (fvar c) ((const 2))) (fvar d)))))))
	(if3 (applic (fvar f) ((applic (fvar f) ((const 1)))))
	      (if3 (applic
		    (fvar a)
		    ((applic
			(fvar a)
			((applic
			  (fvar b)
			  ((applic (fvar c) ((const 2))) (fvar d)))))))
		  (if3 (applic (fvar g) ((const 2)))
			(if3 (applic (fvar z) ())
			    (if3 (const 1)
				  (if3 (applic (fvar h) ((const 3)))
				      (applic
					(fvar a)
					((applic
					    (fvar a)
					    ((applic
					      (fvar b)
					      ((applic (fvar c) ((const 2)))
						(fvar d)))))))
				      (const #f))
				  (const #f))
			    (const #f))
			(const #f))
		  (const #f))
	      (const #f))
	(const #f)))
	
    (c '(let ((a 1)) (and (a (a (b (c 2) d))) (f (f 1)) (a (a (b (c 2) d))) (g 2) (z) 1 (h 3) (a (a (b (c 2) d)))))	
       '(applic
	  (lambda-simple
	    (a)
	    (if3 (applic
		  (pvar a 0)
		  ((applic
		      (pvar a 0)
		      ((applic
			(fvar b)
			((applic (fvar c) ((const 2))) (fvar d)))))))
		(if3 (applic (fvar f) ((applic (fvar f) ((const 1)))))
		      (if3 (applic
			    (pvar a 0)
			    ((applic
				(pvar a 0)
				((applic
				  (fvar b)
				  ((applic (fvar c) ((const 2))) (fvar d)))))))
			  (if3 (applic (fvar g) ((const 2)))
				(if3 (applic (fvar z) ())
				    (if3 (const 1)
					  (if3 (applic (fvar h) ((const 3)))
					      (tc-applic
						(pvar a 0)
						((applic
						    (pvar a 0)
						    ((applic
						      (fvar b)
						      ((applic
							  (fvar c)
							  ((const 2)))
							(fvar d)))))))
					      (const #f))
					  (const #f))
				    (const #f))
				(const #f))
			  (const #f))
		      (const #f))
		(const #f)))
	  ((const 1))))
	  
    (c '(begin (a (a (b (c 2) d))) (f 1) (g 2) (z) 1 (h 3) (a (a (b (c 2) d))))
       '(seq ((applic
        (fvar a)
        ((applic
           (fvar a)
           ((applic
              (fvar b)
              ((applic (fvar c) ((const 2))) (fvar d))))))) (applic (fvar f) ((const 1))) (applic (fvar g) ((const 2)))
       (applic (fvar z) ()) (const 1) (applic (fvar h) ((const 3)))
       (applic
         (fvar a)
         ((applic
            (fvar a)
            ((applic
               (fvar b)
               ((applic (fvar c) ((const 2))) (fvar d))))))))))
               
    (c '(((lambda (a b c d) (begin (a (a (b (c 2) d))) (f 1) (g 2) (z) 1 (h 3) (a (a (b (c 2) ((lambda () ((lambda () d)))))))))))
       '(applic
	  (applic
	    (lambda-simple
	      (a b c d)
	      (seq ((applic
		      (pvar a 0)
		      ((applic
			(pvar a 0)
			((applic
			    (pvar b 1)
			    ((applic (pvar c 2) ((const 2))) (pvar d 3))))))) (applic (fvar f) ((const 1))) (applic (fvar g) ((const 2)))
		    (applic (fvar z) ()) (const 1) (applic (fvar h) ((const 3)))
		    (tc-applic
		      (pvar a 0)
		      ((applic
			  (pvar a 0)
			  ((applic
			    (pvar b 1)
			    ((applic (pvar c 2) ((const 2))) (pvar d 3))))))))))
	    ())
	  ()))

   (c '((lambda () 5))
      '(const 5))
      
   (c '(lambda (x) (x x) (display "asaf"))
      '(lambda-simple
	(x)
	(seq ((applic (pvar x 0) ((pvar x 0)))
	      (tc-applic (fvar display) ((const "asaf")))))))

   (c '(lambda (f) ((lambda (x) (f (lambda s (apply (x x) s)))) (lambda (x) (f (lambda s (apply (x x) s))))))
      '(lambda-simple
	(f)
	(tc-applic
	  (lambda-simple
	    (x)
	    (tc-applic
	      (bvar f 0 0)
	      ((lambda-var
		s
		(tc-applic
		  (fvar apply)
		  ((applic (bvar x 0 0) ((bvar x 0 0))) (pvar s 0)))))))
	  ((lambda-simple
	    (x)
	    (tc-applic
	      (bvar f 0 0)
	      ((lambda-var
		  s
		  (tc-applic
		    (fvar apply)
		    ((applic (bvar x 0 0) ((bvar x 0 0))) (pvar s 0)))))))))))
		    
    (c '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))		    
       '(applic
	  (fvar x)
	  ((lambda-simple
	    (x)
	    (tc-applic
	      (pvar x 0)
	      ((lambda-simple
		  ()
		  (tc-applic
		    (bvar x 0 0)
		    ((lambda-simple
		      ()
		      (tc-applic (bvar x 1 0) ((bvar x 1 0)))))))))))))
	
    (c '(lambda (x) (x x))
       '(lambda-simple (x) (tc-applic (pvar x 0) ((pvar x 0)))))
	
   (c '((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))
      '(applic (fvar +) ()))
      
   (c '(((lambda () f)) ((lambda () g)) ((lambda () h)) (z (m c (d ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () ((lambda () (+)))))))))))))))))))
      '(applic
	(fvar f)
	((fvar g)
	  (fvar h)
	  (applic
	    (fvar z)
	    ((applic
	      (fvar m)
	      ((fvar c) (applic (fvar d) ((applic (fvar +) ()))))))))))
   
))  

(define GiladWinterfeldTests
  (l
        ;;test1
        (c '(lambda (f) ((lambda (x) (f (lambda s (apply (x x) s)))) (lambda (x) (f (lambda s (apply (x x) s))))))
	   '(lambda-simple
                (f)
                (tc-applic
                    (lambda-simple
                    (x)
                    (tc-applic
                        (bvar f 0 0)
                        ((lambda-var
                        s
                        (tc-applic
                            (fvar apply)
                            ((applic (bvar x 0 0) ((bvar x 0 0))) (pvar s 0)))))))
                    ((lambda-simple
                    (x)
                    (tc-applic
                        (bvar f 0 0)
                        ((lambda-var
                            s
                            (tc-applic
                            (fvar apply)
                            ((applic (bvar x 0 0) ((bvar x 0 0))) (pvar s 0)))))))))))
        ;;test2
        (c '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))
            '(applic
                (fvar x)
                ((lambda-simple
                    (x)
                    (tc-applic
                    (pvar x 0)
                    ((lambda-simple
                        ()
                        (tc-applic
                            (bvar x 0 0)
                            ((lambda-simple
                            ()
                            (tc-applic (bvar x 1 0) ((bvar x 1 0)))))))))))))        

        ;;test3
        (c '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))
	   '(def (fvar fact)
                (lambda-simple
                (n)
                (if3 (applic (fvar zero?) ((pvar n 0)))
                        (const 1)
                        (tc-applic
                        (fvar *)
                        ((pvar n 0)
                            (applic
                            (fvar fact)
                            ((applic (fvar -) ((pvar n 0) (const 1)))))))))))
                            
        ;;test4
        (c '(lambda (x) (x x))
	   '(lambda-simple (x) (tc-applic (pvar x 0) ((pvar x 0)))))

        ;;test5
        (c '(define fact (lambda (n) (if (zero? n) 1 (* n (fact (- n 1))))))
           '(def (fvar fact)
                (lambda-simple
                (n)
                (if3 (applic (fvar zero?) ((pvar n 0)))
                        (const 1)
                        (tc-applic
                        (fvar *)
                        ((pvar n 0)
                            (applic
                            (fvar fact)
                            ((applic (fvar -) ((pvar n 0) (const 1)))))))))))

        ;;test6
        (c '(lambda (a  b) (lambda (c) (+ a b c)))
            '(lambda-simple
                (a b)
                (lambda-simple
                    (c)
                    (tc-applic
                    (fvar +)
                    ((bvar a 0 0) (bvar b 0 1) (pvar c 0))))))           

        ;;test7
        (c '(x (lambda (x) (x (lambda () (x (lambda () (x x)))))))
            '(applic
                (fvar x)
                ((lambda-simple
                    (x)
                    (tc-applic
                    (pvar x 0)
                    ((lambda-simple
                        ()
                        (tc-applic
                            (bvar x 0 0)
                            ((lambda-simple
                            ()
                            (tc-applic (bvar x 1 0) ((bvar x 1 0)))))))))))))        

        ;;test8
        (c '(let ((a 0)) (list (lambda () a) (lambda () (set! a (+ a 1))) (lambda (b) (set! a b))))
            '(applic
                (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (box (pvar a 0)))
                        (tc-applic
                            (fvar list)
                            ((lambda-simple () (box-get (bvar a 0 0)))
                            (lambda-simple
                                ()
                                (box-set
                                (bvar a 0 0)
                                (applic (fvar +) ((box-get (bvar a 0 0)) (const 1)))))
                            (lambda-simple (b) (box-set (bvar a 0 0) (pvar b 0))))))))
                ((const 0))))        

        ;;test9
        (c '(define my-even? (lambda (e) (define even? (lambda (n) (or (zero? n) (odd? (- n 1))))) (define odd? (lambda (n) (and (positive? n) (even? (- n 1))))) (even? e)))
            '(def (fvar my-even?)
                (lambda-simple
                (e)
                (tc-applic
                    (lambda-simple
                    (even? odd?)
                    (seq ((set (pvar even? 0) (box (pvar even? 0)))
                            (set (pvar odd? 1) (box (pvar odd? 1)))
                            (box-set
                                (pvar even? 0)
                                (lambda-simple
                                (n)
                                (or ((applic (fvar zero?) ((pvar n 0)))
                                        (tc-applic
                                        (box-get (bvar odd? 0 1))
                                        ((applic
                                            (fvar -)
                                            ((pvar n 0) (const 1)))))))))
                            (box-set
                                (pvar odd? 1)
                                (lambda-simple
                                (n)
                                (if3 (applic (fvar positive?) ((pvar n 0)))
                                    (tc-applic
                                        (box-get (bvar even? 0 0))
                                        ((applic (fvar -) ((pvar n 0) (const 1)))))
                                    (const #f))))
                            (tc-applic (box-get (pvar even? 0)) ((bvar e 0 0))))))
                    ((const #f) (const #f))))))
        
        ;;test10
        (c '(+ 1 2)
            '(applic (fvar +) ((const 1) (const 2))))        
        
        ;;test11
        (c '(lambda (x . y) (lambda (x) y x (set! x 1 )))
           '(lambda-opt  (x)  y  (lambda-simple    (x)    (seq ((bvar y 0 1) (pvar x 0) (set (pvar x 0) (const 1)))))))
        
        ;;test12
        (c '(lambda (x  y) (lambda () y x (set! x 1 )))
            '(lambda-simple (x y) (seq ((set (pvar x 0) (box (pvar x 0))) (lambda-simple () (seq ((bvar y 0 1) (box-get (bvar x 0 0)) (box-set (bvar x 0 0) (const 1)))))))))        
        
        ;;test13
        (c '(lambda (x ) (lambda (x) y  (set! x 1 )))
            '(lambda-simple
                (x)
                (lambda-simple
                    (x)
                    (seq ((fvar y) (set (pvar x 0) (const 1)))))))        
        
        ;;test14
        (c '(lambda (x  ) (lambda (x) y x (set! z 1 )))
            '(lambda-simple
                (x)
                (lambda-simple
                    (x)
                    (seq ((fvar y) (pvar x 0) (set (fvar z) (const 1)))))))        
        
        ;;test15
        (c '(lambda (x ) (lambda x  x (set! x 1 )))
            '(lambda-simple
                (x)
                (lambda-var
                    x
                    (seq ((pvar x 0) (set (pvar x 0) (const 1)))))))        
        
        ;;test16
        (c '(lambda (x ) x (lambda (a b) (set! x 1 )))
            '(lambda-simple
                (x)
                (seq ((set (pvar x 0) (box (pvar x 0)))
                        (box-get (pvar x 0))
                        (lambda-simple (a b) (box-set (bvar x 0 0) (const 1)))))))
                        
        ;;test17
        (c '(lambda x x (lambda (a b) (set! x 1 )))
           '(lambda-var
                x
                (seq ((set (pvar x 0) (box (pvar x 0)))
                        (box-get (pvar x 0))
                        (lambda-simple (a b) (box-set (bvar x 0 0) (const 1)))))))        
        
        ;'(lambda x a (lambda (a b) (set! x 1 )))
        
        ;;test18
        (c '(lambda (a) (+ x a (lambda b (+ x a b (lambda (c . d) (+ x a b c d (lambda (e f g) (+ x a b c d e f g))))))))
            '(lambda-simple
                (a)
                (tc-applic
                    (fvar +)
                    ((fvar x)
                    (pvar a 0)
                    (lambda-var
                        b
                        (tc-applic
                        (fvar +)
                        ((fvar x)
                            (bvar a 0 0)
                            (pvar b 0)
                            (lambda-opt
                            (c)
                            d
                            (tc-applic
                                (fvar +)
                                ((fvar x) (bvar a 1 0) (bvar b 0 0) (pvar c 0) (pvar d 1)
                                (lambda-simple
                                    (e f g)
                                    (tc-applic
                                    (fvar +)
                                    ((fvar x) (bvar a 2 0) (bvar b 1 0) (bvar c 0 0)
                                        (bvar d 0 1) (pvar e 0) (pvar f 1)
                                        (pvar g 2))))))))))))))        
        ;;test19
        (c '(+ 1 2 (lambda () (if (+ (- 1)) (if ( + (set! a (+ 1))) (or (+ 1) (+ 2) ) (begin (+ 1) (lambda () (+ 2)) (+ 3))) (lambda a (+ a)))))
            '(applic
                (fvar +)
                ((const 1)
                    (const 2)
                    (lambda-simple
                    ()
                    (if3 (applic (fvar +) ((applic (fvar -) ((const 1)))))
                        (if3 (applic
                                (fvar +)
                                ((set (fvar a) (applic (fvar +) ((const 1))))))
                                (or ((applic (fvar +) ((const 1)))
                                    (tc-applic (fvar +) ((const 2)))))
                                (seq ((applic (fvar +) ((const 1)))
                                    (lambda-simple () (tc-applic (fvar +) ((const 2))))
                                    (tc-applic (fvar +) ((const 3))))))
                        (lambda-var a (tc-applic (fvar +) ((pvar a 0)))))))))
                        
        ;;test20
        (c '((lambda () (+ ((lambda () a)) ((lambda () b)) ((lambda () ((lambda () c)))))))
           '(applic (fvar +) ((fvar a) (fvar b) (fvar c))))
        
        ;;test21
        (c '(+ a b c (lambda (a b c) (+ a b c (lambda (a) (+ a b c (lambda(b) (+ a b c (lambda(c) (+ a b c (lambda (x) a b c) )))))))))
           '(applic
                (fvar +)
                ((fvar a)
                    (fvar b)
                    (fvar c)
                    (lambda-simple
                    (a b c)
                    (tc-applic
                        (fvar +)
                        ((pvar a 0)
                        (pvar b 1)
                        (pvar c 2)
                        (lambda-simple
                            (a)
                            (tc-applic
                            (fvar +)
                            ((pvar a 0)
                                (bvar b 0 1)
                                (bvar c 0 2)
                                (lambda-simple
                                (b)
                                (tc-applic
                                    (fvar +)
                                    ((bvar a 0 0)
                                    (pvar b 0)
                                    (bvar c 1 2)
                                    (lambda-simple
                                        (c)
                                        (tc-applic
                                        (fvar +)
                                        ((bvar a 1 0)
                                            (bvar b 0 0)
                                            (pvar c 0)
                                            (lambda-simple
                                            (x)
                                            (seq ((bvar a 2 0)
                                                    (bvar b 1 0)
                                                    (bvar c 0 0)))))))))))))))))))
        ;;test22
        (c '(+ a b c (lambda a (+ a b c (lambda (a . b) (+ a b c (lambda (a) (+ a b c (lambda a (+ a b c (lambda (a . b) (+ a b c (lambda (a) (+ a b c)))))))))))))
           '(applic
            (fvar +)
            ((fvar a)
                (fvar b)
                (fvar c)
                (lambda-var
                a
                (tc-applic
                    (fvar +)
                    ((pvar a 0)
                    (fvar b)
                    (fvar c)
                    (lambda-opt
                        (a)
                        b
                        (tc-applic
                        (fvar +)
                        ((pvar a 0)
                            (pvar b 1)
                            (fvar c)
                            (lambda-simple
                            (a)
                            (tc-applic
                                (fvar +)
                                ((pvar a 0)
                                (bvar b 0 1)
                                (fvar c)
                                (lambda-var
                                    a
                                    (tc-applic
                                    (fvar +)
                                    ((pvar a 0)
                                        (bvar b 1 1)
                                        (fvar c)
                                        (lambda-opt
                                        (a)
                                        b
                                        (tc-applic
                                            (fvar +)
                                            ((pvar a 0)
                                            (pvar b 1)
                                            (fvar c)
                                            (lambda-simple
                                                (a)
                                                (tc-applic
                                                (fvar +)
                                                ((pvar a 0)
                                                    (bvar b 0 1)
                                                    (fvar c))))))))))))))))))))))
                                                    
        ;;test23
        (c '(lambda(x)  (set! x 1) (lambda() (set! t (+ x 1) )))
           '(lambda-simple
                (x)
                (seq ((set (pvar x 0) (box (pvar x 0)))
                        (box-set (pvar x 0) (const 1))
                        (lambda-simple
                        ()
                        (set (fvar t)
                                (applic (fvar +) ((box-get (bvar x 0 0)) (const 1)))))))))          
        
        ;;test24
        (c '(lambda () (define a 1) (lambda() (define b 2) 'body0) )
           '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (const 1))
                            (lambda-simple
                            ()
                            (tc-applic
                                (lambda-simple
                                (b)
                                (seq ((set (pvar b 0) (const 2)) (const body0))))
                                ((const #f)))))))
                    ((const #f)))))        
        
        ;;test25
        (c '(lambda (a . b) (define x 1) 1)
           '(lambda-opt
                (a)
                b
                (tc-applic
                    (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1)) (const 1))))
                    ((const #f)))))        
        
        ;;test26
        (c '(lambda a (define x 1) x)
           '(lambda-var
                a
                (tc-applic
                    (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1)) (pvar x 0))))
                    ((const #f)))))          
        
        ;;test27
        (c '(lambda (a  b ) (define x 1) (lambda (a) (define a 4) (lambda a (define a 3) 1)))
           '(lambda-simple
                (a b)
                (tc-applic
                    (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1))
                            (lambda-simple
                            (a)
                            (tc-applic
                                (lambda-simple
                                (a)
                                (seq ((set (pvar a 0) (const 4))
                                        (lambda-var
                                            a
                                            (tc-applic
                                            (lambda-simple
                                                (a)
                                                (seq ((set (pvar a 0) (const 3))
                                                    (const 1))))
                                            ((const #f)))))))
                                ((const #f)))))))
                    ((const #f)))))
                    
        ;;test28
        (c '(lambda (a) (define p (lambda (ab) (define s 1) a)) b)
           '(lambda-simple
                (a)
                (tc-applic
                    (lambda-simple
                    (p)
                    (seq ((set (pvar p 0)
                                (lambda-simple
                                (ab)
                                (tc-applic
                                    (lambda-simple
                                    (s)
                                    (seq ((set (pvar s 0) (const 1)) (bvar a 2 0))))
                                    ((const #f)))))
                            (fvar b))))
                    ((const #f)))))
        
        ;;test29
        (c '(if (lambda (a . rr) (define a (lambda z (define z 1) 2)) 3) (+ (begin (+ 4) (lambda (c t) (define r 1) (define g 2) ((lambda() 'hello))))) (lambda () (or (+ 5) (+ (- 6)) (if (+ 6) (set! a (+ 4)) (or ( + 7) (+ ( - 8)))))))
           '(if3 (lambda-opt
                (a)
                rr
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0)
                                (lambda-var
                                    z
                                    (tc-applic
                                    (lambda-simple
                                        (z)
                                        (seq ((set (pvar z 0) (const 1)) (const 2))))
                                    ((const #f)))))
                            (const 3))))
                    ((const #f))))
                (applic
                (fvar +)
                ((seq ((applic (fvar +) ((const 4)))
                        (lambda-simple
                            (c t)
                            (tc-applic
                            (lambda-simple
                                (r g)
                                (seq ((set (pvar r 0) (const 1))
                                        (set (pvar g 1) (const 2))
                                        (const hello))))
                            ((const #f) (const #f))))))))
                (lambda-simple
                ()
                (or ((applic (fvar +) ((const 5)))
                        (applic (fvar +) ((applic (fvar -) ((const 6)))))
                        (if3 (applic (fvar +) ((const 6)))
                            (set (fvar a) (applic (fvar +) ((const 4))))
                            (or ((applic (fvar +) ((const 7)))
                                    (tc-applic
                                    (fvar +)
                                    ((applic (fvar -) ((const 8)))))))))))))
        ;;test30
        (c '(lambda () (define (a x) x) b)
           '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (lambda-simple (x) (pvar x 0)))
                            (fvar b))))
                    ((const #f)))))
                    
        ;;test31
        (c '(lambda () (define x 1) (define (x) 2) 'body)
           '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (x x)
                    (seq ((set (pvar x 0) (const 1))
                            (set (pvar x 0) (lambda-simple () (const 2)))
                            (const body))))
                    ((const #f) (const #f)))))
                    
        ;;test32
        (c '((lambda a a))
           '(applic (lambda-var a (pvar a 0)) ()))
            
        ;;test33
        (c '((lambda () (define x 1) x))
           '(applic
                (lambda-simple
                    (x)
                    (seq ((set (pvar x 0) (const 1)) (pvar x 0))))
                ((const #f))))
                
        ;;test34
        (c '(lambda () (define (a . b) a) 1)
           '(lambda-simple
                ()
                (tc-applic
                    (lambda-simple
                    (a)
                    (seq ((set (pvar a 0) (box (pvar a 0)))
                            (box-set (pvar a 0) (lambda-var b (box-get (bvar a 0 0))))
                            (const 1))))
                    ((const #f)))))        
        
        ;;test35
        (c '(let* ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))
           '(applic
                (lambda-simple
                    (a)
                    (tc-applic
                    (lambda-simple
                        (b)
                        (tc-applic
                        (lambda-simple
                            (c)
                            (tc-applic
                            (lambda-simple
                                (d)
                                (tc-applic
                                (fvar +)
                                ((bvar a 2 0) (bvar b 1 0) (bvar c 0 0) (pvar d 0))))
                            ((const 4))))
                        ((const 3))))
                    ((const 2))))
                ((const 1))))        
        
        ;;test36
        (c '(letrec ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))
           '(applic
                (lambda-simple
                    (a b c d)
                    (seq ((set (pvar a 0) (const 1))
                        (set (pvar b 1) (const 2))
                        (set (pvar c 2) (const 3))
                        (set (pvar d 3) (const 4))
                        (tc-applic
                            (fvar +)
                            ((pvar a 0) (pvar b 1) (pvar c 2) (pvar d 3))))))
                ((const #f) (const #f) (const #f) (const #f))))
                
        ;;test37
        (c '(let ([a 1] [b 2] [c 3] [d 4]) (+ a b c d))
           '(applic
                (lambda-simple
                    (a b c d)
                    (tc-applic
                    (fvar +)
                    ((pvar a 0) (pvar b 1) (pvar c 2) (pvar d 3))))
                ((const 1) (const 2) (const 3) (const 4))))        
        
        ;;test38
        (c '(let ([a (let ([b 1]) (define b 2) b)]) (define a 1) a)
           '(applic
                (lambda-simple
                    (a)
                    (tc-applic
                    (lambda-simple
                        (a)
                        (seq ((set (pvar a 0) (const 1)) (pvar a 0))))
                    ((const #f))))
                ((applic
                    (lambda-simple
                    (b)
                    (tc-applic
                        (lambda-simple
                        (b)
                        (seq ((set (pvar b 0) (const 2)) (pvar b 0))))
                        ((const #f))))
                    ((const 1))))))        

        ;;test39
        (c '(lambda () (or (+ 1) (or ( + 2) (+ 3) ) (+ 4)))
           '(lambda-simple
                ()
                (or ((applic (fvar +) ((const 1)))
                        (or ((applic (fvar +) ((const 2)))
                            (applic (fvar +) ((const 3)))))
                        (tc-applic (fvar +) ((const 4)))))))
                        
        ;;test40
        (c '(or (+ 1) (or ( + 2) (+ 3) ) (+ 4))
           '(or ((applic (fvar +) ((const 1)))
                (or ((applic (fvar +) ((const 2)))
                        (applic (fvar +) ((const 3)))))
                (applic (fvar +) ((const 4))))))
                
        ;;test41  (negative test)
        (c '((lambda () 1) 2 3 4)
           '(applic
                (lambda-simple () (const 1))
                ((const 2) (const 3) (const 4))))        

))        

(display (format "\033[1mComp171 - Ass3 Tests\033[0m\n================================\n"))

(runAllTests
  (list
      (cons "Gilad Winterfeld Tests" GiladWinterfeldTests) 
      (cons "Tests" Tests)           
))