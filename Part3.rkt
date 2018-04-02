; If you are using racket instead of scheme, uncomment these two lines, comment the (load "simpleParser.scm") and uncomment the (require "simpleParser.scm")
; #lang racket
; (require "simpleParser.scm")
(load "functionParser.scm")
(require racket/trace)

; Huvra: W 5:42p >> Updated functionParser.scm
; Peter: S 1:07a >> I'm back
; Raza: M 0:51p >> There's an error but the main error is that eval-expression only evaluates arithmetic expression and it cannot do functions.

; An interpreter for the simple language that uses call/cc for the continuations.  Does not handle side effects.
;(define call/cc call-with-current-continuation)


; The functions that start interpret-...  all return the current environment. << enviornment is referencing the "state"
; The functions that start eval-...  all return a value

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.
(define interpret
  (lambda (file)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (append (parser file) '((return (funcall main)))) (newenvironment) return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))

; interprets a list of statements.  The environment from each statement is used for the next ones.
(define interpret-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment ; return the state/enviornment
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))
        ; if the parse list is not empty then send to the main function that will sort where we need to go based on which key word we see.

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return)) ; return
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment)) ; variable creation
      ((eq? '= (statement-type statement)) (interpret-assign statement environment)) ; assign variable
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw)) ;if 
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment)) ; continue
      ((eq? 'break (statement-type statement)) (break environment)) ; break
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw)) ; begin << creates a new layer in the enviornment
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment throw)) ; throw
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw)) ; try-catch-finally
      ((eq? 'function (statement-type statement)) (interpret-function statement environment return break continue throw)); defines the functions (add binding)

      ((eq? 'funcall (statement-type statement)) (interpret-funcall statement environment return break continue throw)); ??? reuturn break continue throw)); call or runs the functions from bindings
                       
      (else (myerror "Unknown statement:" (statement-type statement)))))) ; error

;;;;;;;;;;;;;;;;;;; FUNCTION BIND ;;;;;;;;;;;;;;;;;;; Peter 1:12a

(define interpret-function
   (lambda (statement environment return break continue throw)
     (cond
       ((null? statement) (error "Mistake?"))
       ((insert (f_name statement)
                (list (f_parameters statement) (f_body statement) (lambda (state) (lim-env (f_name statement) state))) environment)))))

;; abstractions
(define f_name ;function name 
  (lambda (statement)
    (cadr statement))) 
(define f_parameters ;function parameters
  (lambda (statement)
    (caddr statement))) 
(define f_body ;function body 
  (lambda (statement)
    (cadddr statement))) 

; Establish scope of the function
(define lim-env
  (lambda (f_name state)
    (if (null? state)
        (error "Function name not found.")
          (if (eq? (value f_name state) 'not_found)
              (trim-state funcname (cdr state))
              state))))

; abstractions
(define value
  (lambda (f_name state)
    (lookup-in-frame f_name (car state))))

;;;;;;;;;;;;;;;;;;; FUNCTION CALL ;;;;;;;;;;;;;;;;;;;

; will be its own thing

;Raza 1st April 2021 11:35. The following functions are imitated from a correctly working code so the author
;doesn't know of how exactly they function. Giving an error at err but it might be due to other problems.

(define interpret-funcall
  (lambda (statement environment return break continue throw)
    (call/cc
       (lambda (return)
         (interpret-statement-list (cadr (closure (f_name statement) environment)) (newstate statement environment return break continue throw) return break continue throw)))))

(define closure ;get the closure
  (lambda (f_name environment)
    (cond
      ((null? environment) (error "Error: Attempted to call a function that has not been initialized in scope." fun-name))
      ((exists? f_name environment) (lookup f_name environment))
      (else (error "do I have to remove a layer or not?" ) )))) ;(lookup f_name (cdr environment)))))

;for the first test only main function is called so the state returned will be empty which is appended in newstate
(define actual_param_layer
    (lambda (formal actual state return break continue)
      '(() ())))

(define add-to-layer
  (lambda (layer var value)
    (list (cons var (car layer)) (cons value (cadr layer)))))

(define newstate
  (lambda (statement environment return break continue throw)
    (cons (actual_param_layer (car (closure (f_name statement) environment)) (cddr statement) environment return break continue) ;returns an empty state right now
                           ( (caddr (closure (f_name statement) environment)) environment))))

;;;;;;;;;;;;;;;;;;; RETURN ;;;;;;;;;;;;;;;;;;;

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return)
    (return (eval-expression (get-expr statement) environment))))

;;;;;;;;;;;;;;;;;;; Variable Declare ;;;;;;;;;;;;;;;;;;;

; Adds a new variable binding to the environment.  There may be an assignment with the variable
(define interpret-declare
  (lambda (statement environment)
    (if (exists-declare-value? statement) (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment) environment)
        (insert (get-declare-var statement) 'novalue environment))))

;;;;;;;;;;;;;;;;;;; Assignment ;;;;;;;;;;;;;;;;;;;

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment) environment)))

;;;;;;;;;;;;;;;;;;; If ;;;;;;;;;;;;;;;;;;;

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

;;;;;;;;;;;;;;;;;;; While ;;;;;;;;;;;;;;;;;;;

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment)
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

;;;;;;;;;;;;;;;;;;; Block/Begin ;;;;;;;;;;;;;;;;;;;

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment return break continue throw)
    (pop-frame (interpret-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

;;;;;;;;;;;;;;;;;;; Throw & Try-Catch-Finally ;;;;;;;;;;;;;;;;;;;

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment throw)
    (throw (eval-expression (get-expr statement) environment) environment)))

; Interpret a try-catch-finally block

; Create a continuation for the throw.  If there is no catch, it has to interpret the finally block, and once that completes throw the exception.
;   Otherwise, it interprets the catch block with the exception bound to the thrown value and interprets the finally block when the catch is done
(define create-throw-catch-continuation
  (lambda (catch-statement environment return break continue throw jump finally-block)
    (cond
      ((null? catch-statement) (lambda (ex env) (throw ex (interpret-block finally-block env return break continue throw)))) 
      ((not (eq? 'catch (statement-type catch-statement))) (myerror "Incorrect catch statement"))
      (else (lambda (ex env)
              (jump (interpret-block finally-block
                                     (pop-frame (interpret-statement-list 
                                                 (get-body catch-statement) 
                                                 (insert (catch-var catch-statement) ex (push-frame env))
                                                 return 
                                                 (lambda (env2) (break (pop-frame env2))) 
                                                 (lambda (env2) (continue (pop-frame env2))) 
                                                 (lambda (v env2) (throw v (pop-frame env2)))))
                                     return break continue throw)))))))

; To interpret a try block, we must adjust  the return, break, continue continuations to interpret the finally block if any of them are used.
;  We must create a new throw continuation and then interpret the try block with the new continuations followed by the finally block with the old continuations
(define interpret-try
  (lambda (statement environment return break continue throw)
    (call/cc
     (lambda (jump)
       (let* ((finally-block (make-finally-block (get-finally statement)))
              (try-block (make-try-block (get-try statement)))
              (new-return (lambda (v) (begin (interpret-block finally-block environment return break continue throw) (return v))))
              (new-break (lambda (env) (break (interpret-block finally-block env return break continue throw))))
              (new-continue (lambda (env) (continue (interpret-block finally-block env return break continue throw))))
              (new-throw (create-throw-catch-continuation (get-catch statement) environment return break continue throw jump finally-block)))
         (interpret-block finally-block
                          (interpret-block try-block environment new-return new-break new-continue new-throw)
                          return break continue throw))))))

; helper methods so that I can reuse the interpret-block method on the try and finally blocks
(define make-try-block
  (lambda (try-statement)
    (cons 'begin try-statement)))

(define make-finally-block
  (lambda (finally-statement)
    (cond
      ((null? finally-statement) '(begin))
      ((not (eq? (statement-type finally-statement) 'finally)) (myerror "Incorrectly formatted finally block"))
      (else (cons 'begin (cadr finally-statement))))))

;;;;;;;;;;;;;;;;;;; M_expression Broken Into Two ;;;;;;;;;;;;;;;;;;;

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment)
    (cond
      
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((and (list? expr) (eq? 'funcall (operator expr))) (interpret-funcall expr environment null null null null))
      ;((eq? 'funcall (operator expr)) (interpret-funcall expr environment null null null null))
      ((not (list? expr)) (lookup expr environment))
      (else (eval-operator expr environment)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment) environment)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment)))
      ;((eq? 'funcall (operator expr)) (myerror "good start"))
      (else (myerror "Unknown operator:" (operator expr))))))

; Determines if two values are equal.  We need a special test because there are both boolean and integer types.
(define isequal
  (lambda (val1 val2)
    (if (and (number? val1) (number? val2))
        (= val1 val2)
        (eq? val1 val2))))


;-----------------
; HELPER FUNCTIONS
;-----------------

; These helper functions define the operator and operands of a value expression
(define operator car)
(define operand1 cadr)
(define operand2 caddr)
(define operand3 cadddr)

(define exists-operand2?
  (lambda (statement)
    (not (null? (cddr statement)))))

(define exists-operand3?
  (lambda (statement)
    (not (null? (cdddr statement)))))

; these helper functions define the parts of the various statement types
(define statement-type operator)
(define get-expr operand1)
(define get-declare-var operand1)
(define get-declare-value operand2)
(define exists-declare-value? exists-operand2?)
(define get-assign-lhs operand1)
(define get-assign-rhs operand2)
(define get-condition operand1)
(define get-then operand2)
(define get-else operand3)
(define get-body operand2)
(define exists-else? exists-operand3?)
(define get-try operand1)
(define get-catch operand2)
(define get-finally operand3)

(define catch-var
  (lambda (catch-statement)
    (car (operand1 catch-statement))))


;------------------------
; Environment/State Functions
;------------------------

; create a new empty environment
(define newenvironment
  (lambda ()
    (list (newframe))))

; create an empty frame: a frame is two lists, the first are the variables and the second is the "store" of values
(define newframe
  (lambda ()
    '(() ())))

; add a frame onto the top of the environment
(define push-frame
  (lambda (environment)
    (cons (newframe) environment)))

; remove a frame from the environment
(define pop-frame
  (lambda (environment)
    (cdr environment)))

; some abstractions
(define topframe car)
(define remainingframes cdr)

; does a variable exist in the environment?
(define exists?
  (lambda (var environment)
    (cond
      ((null? environment) #f)
      ((exists-in-list? var (variables (topframe environment))) #t)
      (else (exists? var (remainingframes environment))))))

; does a variable exist in a list?
(define exists-in-list?
  (lambda (var l)
    (cond
      ((null? l) #f)
      ((eq? var (car l)) #t)
      (else (exists-in-list? var (cdr l))))))

; Looks up a value in the environment.  If the value is a boolean, it converts our languages boolean type to a Scheme boolean type
(define lookup
  (lambda (var environment)
    (lookup-variable var environment)))
  
; A helper function that does the lookup.  Returns an error if the variable does not have a legal value
(define lookup-variable
  (lambda (var environment)
    (let ((value (lookup-in-env var environment)))
      (if (eq? 'novalue value)
          (myerror "error: variable without an assigned value:" var)
          value))))

; Return the value bound to a variable in the environment
(define lookup-in-env
  (lambda (var environment)
    (cond
      ((null? environment) (myerror "error: undefined variable" var))
      ((exists-in-list? var (variables (topframe environment))) (lookup-in-frame var (topframe environment)))
      (else (lookup-in-env var (cdr environment))))))

; Return the value bound to a variable in the frame << lookup layer 
(define lookup-in-frame
  (lambda (var frame)
    (cond
      ((not (exists-in-list? var (variables frame))) (myerror "error: undefined variable" var))
      (else (language->scheme (get-value (indexof var (variables frame)) (store frame)))))))

; Get the location of a name in a list of names
(define indexof
  (lambda (var l)
    (cond
      ((null? l) 0)  ; should not happen
      ((eq? var (car l)) 0)
      (else (+ 1 (indexof var (cdr l)))))))

; Get the value stored at a given index in the list
(define get-value
  (lambda (n l)
    (cond
      ((zero? n) (car l))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (car environment)) (cdr environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (scheme->language val) (store frame)))))

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (if (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (scheme->language val) (cdr vallist)))
      (else (cons (car vallist) (update-in-frame-store var val (cdr varlist) (cdr vallist)))))))

; Returns the list of variables from a frame
(define variables
  (lambda (frame)
    (car frame)))

; Returns the store from a frame
(define store
  (lambda (frame)
    (cadr frame)))

; Functions to convert the Scheme #t and #f to our languages true and false, and back.

(define language->scheme
  (lambda (v) 
    (cond 
      ((eq? v 'false) #f)
      ((eq? v 'true) #t)
      (else v))))

(define scheme->language
  (lambda (v)
    (cond
      ((eq? v #f) 'false)
      ((eq? v #t) 'true)
      (else v))))

; Because the error function is not defined in R5RS scheme, I create my own:
(define error-break (lambda (v) v))
(call-with-current-continuation (lambda (k) (set! error-break k)))

(define myerror
  (lambda (str . vals)
    (letrec ((makestr (lambda (str vals)
                        (if (null? vals)
                            str
                            (makestr (string-append str (string-append " " (symbol->string (car vals)))) (cdr vals))))))
      (error-break (display (string-append str (makestr "" vals)))))))

(trace interpret-function)
(trace interpret-funcall)
(trace interpret)

(parser "basic.java") 
(interpret "basic.java")
(parser "test1.java") ; return 10 
(interpret "test1.java")
(parser "test2.java") ; return 14
(interpret "test2.java")
(parser "test3.java") ; return 45
(interpret "test3.java")
(parser "test4.java") ; return 55
(interpret "test4.java")
;(parser "test5.java") ; return 1
;(interpret "test5.java")
;(parser "test6.java") ; return 115
;(interpret "test6.java")
;(parser "test7.java") ; return true
;(interpret "test7.java")
;(parser "test8.java") ; return 20
;(interpret "test8.java")
;(parser "test9.java") ; return 24
;(interpret "test9.java")
;(parser "test10.java") ; return 2
;(interpret "test10.java")
;(parser "test11.java") ; return 35
;(interpret "test11.java")
;(parser "test12.java") ; return error
;(interpret "test12.java")
;(parser "test13.java") ; return 90
;(interpret "test13.java")
;(parser "test14.java") ; return 69
;(interpret "test14.java")
;(parser "test15.java") ; return 87
;(interpret "test15.java")
;(parser "test16.java") ; return 64
;(interpret "test16.java")
;(parser "test17.java") ; return error
;(interpret "test17.java")
;(parser "test18.java") ; return 125
;(interpret "test18.java")
;(parser "test19.java") ; return 100
;(interpret "test19.java")
;(parser "test20.java") ; return 2000400
;(interpret "test20.java")
;(parser "test21.java") ; return 3421
;(interpret "test21.java")
;(parser "test22.java") ; return 20332
;(interpret "test22.java")
;(parser "test23.java") ; return 21
;(interpret "test23.java")
