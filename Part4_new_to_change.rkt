; Interpereter Project - Part 4
; Group 29
; Huvra Mehta
; Raza Agha
; Peter Fedrizzi

#lang racket
(require "classParser.scm")

(require racket/trace)

;;;;;;;;; INTERPRET ;;;;;;;;;

; The main function.  Calls parser to get the parse tree and interprets it with a new environment.  The returned value is in the environment.

(define interpret
  (lambda (file class)
    (scheme->language
     (call/cc
      (lambda (return)
        (interpret-statement-list (parser file) (newenvironment) class return
                                  (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
                                  (lambda (v env) (myerror "Uncaught exception thrown"))))))))


; interprets a list of statements.  The environment from each statement is used for the next ones.
; Does not add a layer but runs the statement in order 

;(define interpret-statement-list
;  (lambda (statement-list environment class return break continue throw)
;    (if (null? statement-list)
;        (interpret-main statement-list environment return break continue throw)
;        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment class return break continue throw) class return break continue throw))))
        ; if the parse list is not empty then send to the main function that will sort where we need to go based on which key word we see.

;please change


(define interpret-statement-list
  (lambda (statement-list environment class-name return break continue throw)
    (if (null? statement-list)
        (evaluate-main-class class-name environment return break continue throw)
        ;environment ;keep this for debugging
        (interpret-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) class-name return break continue throw))))



;;;;;;;;; Mstate ;;;;;;;;;

; interpret a statement in the environment with continuations for return, break, continue, throw
(define interpret-statement
  (lambda (statement environment return break continue throw)
    (cond
      ((eq? 'return (statement-type statement)) (interpret-return statement environment return break continue throw)) ; return
      ((eq? 'var (statement-type statement)) (interpret-declare statement environment return break continue throw)) ; variable creation
      ((eq? '= (statement-type statement)) (interpret-assign statement environment return break continue throw)) ; assign variable
      ((eq? 'if (statement-type statement)) (interpret-if statement environment return break continue throw)) ;if 
      ((eq? 'while (statement-type statement)) (interpret-while statement environment return throw))
      ((eq? 'continue (statement-type statement)) (continue environment)) ; continue
      ((eq? 'break (statement-type statement)) (break environment)) ; break
      ((eq? 'begin (statement-type statement)) (interpret-block statement environment return break continue throw)) ; begin << creates a new layer in the enviornment
      ((eq? 'throw (statement-type statement)) (interpret-throw statement environment return break continue throw)) ; throw
      ((eq? 'try (statement-type statement)) (interpret-try statement environment return break continue throw)) ; try-catch-finally

      ((eq? 'function (statement-type statement)) (interpret-function statement environment return break continue throw)); defines the functions (add binding)
     ;((eq? 'funcall (statement-type statement)) (interpret-funcall statement environment return break continue throw)); ??? reuturn break continue throw)); call or runs the functions from bindings

      ; please change maybe not even correct

       ((eq? 'funcall (statement-type statement)) (interpret-funcall-result-environment (cdr (lookup (car (cdr statement)) environment)) (f_actual_parameters (car (lookup (car (cdr statement)) environment)) (cdr (cdr statement)) (push-frame environment) throw)
                                                                                       return
                                                                                       break continue throw))

      ; please change from somewhere
      ((eq? 'dot (statement-type statement)) (interpret-dot (cdr statement) environment return break continue throw))
       
      ((eq? (statement-type statement) 'class) (create-class (cdr statement) environment return break continue throw)) ;create a class closure

      ((eq? (statement-type statement) 'static-function) (interpret-function statement environment return break continue throw)) ;runs static functions
      ((eq? (statement-type statement) 'static-funcall) (interpret-main statement environment return break continue throw))
      ((eq? (statement-type statement) 'new ) (interpret-new-object statement environment return break continue throw))

      ;((eq? (statement-type statement) 'static-var) (interpret-static-funcall statement environment return break continue throw))
      ;((eq? 'dot (statement-type statement)) (interpret-dot statement environment return break continue throw))
                       
      (else (myerror "Unknown statement:" (statement-type statement)))))) ; error

(define interpret-dot
  (lambda (instance-name field-to-lookup environment return break continue throw)
    (cond
      ((null? instance-name) (myerror "instance name was null"))
      ((null? field-to-lookup) (myerror "field-to-lookup was null"))
      (else (eval-expression field-to-lookup (lookup instance-name environment) return break continue throw)))))
           
;;;;;;;; CLASS BIND ;;;;;;;;;;;

;(define create-class
;  (lambda (statement environment class return break continue throw)
;    (cond
;      ((null? statement) (myerror "No class closure"))
;      ;required condition or not?
;      ((null? (c_instances statement)) (insert (c_name statement) (cons  (c_instances statement) (interpret-statement (car (c_methods statement)) environment class return break continue throw)) environment))
;      (else (insert (c_name statement)
;                    (cons  (interpret-statement (c_instances statement) environment class return break continue throw)
;                           (interpret-statement (car (c_methods statement)) environment class return break continue throw))
;                   environment)))))

; new create-class

(define create-class
  (lambda (statement environment return break continue throw)
    (cond
      ((null? statement) (myerror "No class closure"))
      (else (insert (c_name statement) (c_rest statement) environment)))))

(define c_name
  (lambda (statement)
    (car statement)))

(define c_rest
  (lambda (statement)
    (cdr statement)))


;;;;;;;;; FUNCTION BIND ;;;;;;;;;

; Declares the function name and binds it to the function closure 
(define interpret-function
   (lambda (statement environment return break continue throw)
     (cond
       ((null? statement) (error "Mistake?"))
       ((insert (f_name statement)
                (list (f_parameters statement) (f_body statement) (lambda (state) (f_scope statement state))) environment)))))

;; Abstractions
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
(define f_scope
  (lambda (statement enviornment)
    (cond
      ((null? enviornment) enviornment)
      ((null? (cdr enviornment)) enviornment)
      ((exists? (f_name statement) (car enviornment)) enviornment)
      (else (f_scope statement (cdr enviornment))))))

                          
;;;;;;;;; FUNCTION CALL ;;;;;;;;;

; Runs the functions that are called via funcall or main (in the end)

;(define interpret-funcall
;  (lambda (statement environment return break continue throw)
;    (call/cc
;       (lambda (return)
;         (interpret-statement-list (closure_body (closure (f_name statement) environment)) (newstate statement environment return break continue throw) class return break continue throw)))))

;;;please change below

(define interpret-funcall
  (lambda (funcall environment throw)
    (call/cc
     (lambda (return)
       (cond
         ((not (exists? (car funcall) environment)) (myerror "Function does not exist")) ;checks if the function exists
         ((null? (cdr funcall)) (interpret-function-statement-list (cadr (lookup (func-name funcall) environment)) (push-frame (pop-frame environment)) return
                                                                   (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop")) throw)) ; checks if there are parameters
         (else (interpret-function-statement-list (cadr (lookup (func-name funcall) environment))
                                                  (f_actual_parameters (func-name (lookup (func-name funcall) environment)) (parameters funcall) (push-frame environment) throw)
                                                  return (lambda (env) (myerror "Break used outside of loop")) (lambda (env) (myerror "Continue used outside of loop"))
 throw)))))))


; a weird function that relies on so many different helper functions.

(define interpret-funcall-result-environment
 (lambda (statement-list environment return break continue throw)
  (cond
    ((null? statement-list) environment)
    (else (if (list? (call/cc
                      (lambda (breakreturn)
                        (interpret-statement (car statement-list) environment breakreturn break continue throw))))
                  (interpret-funcall-result-environment (cdr statement-list) (call/cc
                                                                              (lambda (breakreturn)
                                                                                (interpret-statement (car statement-list) environment breakreturn break continue throw)))
                                                        return break continue throw)
                  environment)))))

(define func-name car)
(define parameters cdr)
  

; Get the closure
(define closure 
  (lambda (f_name environment)
    (cond
      ((null? environment) (error "Error: Attempted to call a function that has not been initialized in scope." f_name))
      ((exists? f_name environment) (lookup f_name environment))
      (else (error "do I have to remove a layer or not?" ) )))) ;(lookup f_name (cdr environment)))))

; Returns the new state for the function 
(define newstate
  (lambda (statement environment return break continue throw)
    (cons (f_actual_parameters (car (closure (f_name statement) (function-env environment))) (cddr statement) environment return break continue throw)
          (outerenv statement environment))))

;Helper function for outerenv
(define outerenv
  (lambda (statement environment)
   ((caddr (closure (f_name statement) environment)) environment)))


; Returns the state of function. Before it was just empty now hopefully it returns the new state with the actual parameters for the function.
(define f_actual_parameters  
    (lambda (formal actual state return break continue throw)
     (cond
     ((and (null? formal) (null? actual)) '(() ()))
     ((or (null? formal) (null? actual)) (error "Incorrect number of args."))
     ((eq? '& (car formal)) (add-to-layer (f_actual_parameters (cddr formal) (cdr actual) state return break continue throw) (cadr formal) (lookup-in-env state (car actual))))
     (else (add-to-layer (f_actual_parameters (cdr formal) (cdr actual) state return break continue throw) (car formal) (eval-expression (car actual) state return break continue throw))))))

;Taken from somewhere

;I'm assuming funcall is the same as 

;(define add-parameters-to-environment
;  (lambda (param-names param-values environment throw)
;    (cond
;      ((null? param-names) environment)
;      ((not (eq? (length param-names) (length param-values))) (myerror "Mismatching parameters and arguments"))
;      ((list? param-names) (add-parameters-to-environment (parameters param-names) (parameters param-values) (insert (first param-names) (eval-expression (first param-values) (pop-frame environment) throw) environment) throw))
;      (else (insert param-names (eval-expression param-values (pop-frame environment)) environment)))))

(define add-to-layer
  (lambda (layer var value)
    (list (cons var (car layer)) (cons (box value) (cadr layer))))) 

;: Abstractions
(define closure_body
  (lambda (statement)
    (cadr statement)))

;;;;;;;;; EVALUATE MAIN ;;;;;;;;;

(define interpret-main
  (lambda (statement environment class return break continue throw)
    (call/cc
       (lambda (return)
         (interpret-statement-list (closure_body (closure (f_name statement) (function-env environment))) ;(newstate statement environment return break continue throw)
                                   environment class return break continue throw)))))

(define function-env
  (lambda (environment)
    (cdr (unbox (caadar environment)))))

;;;all of the things below have been added.

(define evaluate-main-class
  (lambda (class-name environment return break continue throw)
    (cond
      ((not (exists? class-name environment)) (myerror "Undefined class")) ;this should check if main is associated with a statement list in the env
      (else (interpret-statement-list (cadr (find-function-in-closure (cadr (lookup class-name environment)) 'main))
                                      (make-statelayer-from-instance-fields (cadr (lookup class-name environment)) (push-frame environment) return break continue throw)
                                      class-name return break continue throw)))))

(define find-function-in-closure
  (lambda (class-closure func-name)
    (cond
        ((null? class-closure) (myerror "Function does not exist"))
        ((eq? func-name (cadar class-closure)) (cddar class-closure))
        (else (find-function-in-closure (cdr class-closure) func-name)))))

(define make-statelayer-from-instance-fields
  (lambda (class-closure environment return break continue throw)
    (cond
      ((null? class-closure) environment)
      ((list? (car class-closure)) (make-statelayer-from-instance-fields (cdr class-closure) (interpret-statement (car class-closure) environment return break continue throw) return break continue throw)))))

(define interpret-function-statement-list
  (lambda (statement-list environment return break continue throw)
    (cond 
        ((null? statement-list) (pop-frame environment)) 
        (else (interpret-function-statement-list (cdr statement-list) (interpret-statement (first statement-list) environment return break continue throw) return break continue throw)))))



;;;;;;;;; NEW ;;;;;;;;;


; MIGHT BE DOING THIS WRONG?!?!?!

;(define interpret-new-object
;  (lambda (statement environment return break continue throw)
;    (cond
;      ((state_empty environment) '())
;      (lookup-in-env class environment))))


;;please change

(define interpret-new-object
  (lambda (statement environment return break continue throw)
    (cond
      ((null? statement) (myerror "Statement doesn't exist"))
      ((null? (lookup (cadadr statement) environment)) (myerror "Class doesn't exist"))
      (else (insert (car statement) (make-statelayer-from-instance-fields (get-closure-of-class (lookup (get-new-class-name statement) environment)) (newenvironment) return break continue throw) environment)))))

(define get-new-class-name cadadr) ;returns 'A from '(a (new A))
(define get-closure-of-class cadr) ;returns the closure of the class without what the class extends. Might need to include that later on


;;;;;;;;; RETURN ;;;;;;;;;

; Calls the return continuation with the given expression value
(define interpret-return
  (lambda (statement environment return break continue throw)
    (return (eval-expression (get-expr statement) environment return break continue throw))))

;;;;;;;;; Variable Declare ;;;;;;;;;

; Adds a new variable binding to the environment.  There may be an assignment with the variable

;(define interpret-declare
;  (lambda (statement environment return break continue throw)
;    (if (exists-declare-value? statement) (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment return break continue throw) environment)
;        (insert (get-declare-var statement) 'novalue environment))))

;;please change
(define interpret-declare
  (lambda (statement environment return break continue throw)
    (cond
      ((exists-declare-value? statement) (if (list? (get-declare-value statement))
                                             (interpret-new-object (cdr statement) environment return break continue throw)
                                             (insert (get-declare-var statement) (eval-expression (get-declare-value statement) environment return break continue throw) environment)))
      (else (insert (get-declare-var statement) 'novalue environment)))))
                                         

;;;;;;;;; Assignment ;;;;;;;;;

; Updates the environment to add an new binding for a variable
(define interpret-assign
  (lambda (statement environment return break continue throw)
    (update (get-assign-lhs statement) (eval-expression (get-assign-rhs statement) environment return break continue throw) environment)))

;;;;;;;;; If ;;;;;;;;;

; We need to check if there is an else condition.  Otherwise, we evaluate the expression and do the right thing.
(define interpret-if
  (lambda (statement environment return break continue throw)
    (cond
      ((eval-expression (get-condition statement) environment return break continue throw) (interpret-statement (get-then statement) environment return break continue throw))
      ((exists-else? statement) (interpret-statement (get-else statement) environment return break continue throw))
      (else environment))))

;;;;;;;;; While ;;;;;;;;;

; Interprets a while loop.  We must create break and continue continuations for this loop
(define interpret-while
  (lambda (statement environment return throw)
    (call/cc
     (lambda (break)
       (letrec ((loop (lambda (condition body environment)
                        (if (eval-expression condition environment return null null throw) ;;;;;;;;;;;;;;;;;;; BAD NULLS
                            (loop condition body (interpret-statement body environment return break (lambda (env) (break (loop condition body env))) throw))
                         environment))))
         (loop (get-condition statement) (get-body statement) environment))))))

;;;;;;;;; Block/Begin ;;;;;;;;;

; Interprets a block.  The break, continue, and throw continuations must be adjusted to pop the environment
(define interpret-block
  (lambda (statement environment class return break continue throw)
    (pop-frame (interpret-block-statement-list (cdr statement)
                                         (push-frame environment)
                                         return
                                         (lambda (env) (break (pop-frame env)))
                                         (lambda (env) (continue (pop-frame env)))
                                         (lambda (v env) (throw v (pop-frame env)))))))

(define interpret-block-statement-list
  (lambda (statement-list environment return break continue throw)
    (if (null? statement-list)
        environment
        (interpret-block-statement-list (cdr statement-list) (interpret-statement (car statement-list) environment return break continue throw) return break continue throw))))

;;;;;;;;; Throw & Try-Catch-Finally ;;;;;;;;;

; We use a continuation to throw the proper value. Because we are not using boxes, the environment/state must be thrown as well so any environment changes will be kept
(define interpret-throw
  (lambda (statement environment return break continue throw)
    (throw (eval-expression (get-expr statement) environment return break continue throw) environment)))

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

;;;;;;;;; M_expression ;;;;;;;;;

; Evaluates all possible boolean and arithmetic expressions, including constants and variables.
(define eval-expression
  (lambda (expr environment return break continue throw)
    (cond
      
      ((number? expr) expr)
      ((eq? expr 'true) #t)
      ((eq? expr 'false) #f)
      ((and (list? expr) (eq? 'static-funcall (operator expr))) (interpret-main expr environment return break continue throw))
      ((and (list? expr) (eq? 'funcall (operator expr))) (interpret-funcall expr environment return break continue throw))
      ((not (list? expr)) (lookup expr environment))
      ((eq? (statement-type expr) 'new ) (interpret-new-object expr environment return break continue throw))
      (else (eval-operator expr environment return break continue throw)))))

; Evaluate a binary (or unary) operator.  Although this is not dealing with side effects, I have the routine evaluate the left operand first and then
; pass the result to eval-binary-op2 to evaluate the right operand.  This forces the operands to be evaluated in the proper order in case you choose
; to add side effects to the interpreter
(define eval-operator
  (lambda (expr environment return break continue throw)
    (cond
      ((eq? '! (operator expr)) (not (eval-expression (operand1 expr) environment return break continue throw)))
      ((and (eq? '- (operator expr)) (= 2 (length expr))) (- (eval-expression (operand1 expr) environment return break continue throw)))
      (else (eval-binary-op2 expr (eval-expression (operand1 expr) environment return break continue throw) environment return break continue throw)))))

; Complete the evaluation of the binary operator by evaluating the second operand and performing the operation.
(define eval-binary-op2
  (lambda (expr op1value environment return break continue throw)
    (cond
      ((eq? '+ (operator expr)) (+ op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '- (operator expr)) (- op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '* (operator expr)) (* op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '/ (operator expr)) (quotient op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '% (operator expr)) (remainder op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '== (operator expr)) (isequal op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '!= (operator expr)) (not (isequal op1value (eval-expression (operand2 expr) environment return break continue throw))))
      ((eq? '< (operator expr)) (< op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '> (operator expr)) (> op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '<= (operator expr)) (<= op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '>= (operator expr)) (>= op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '|| (operator expr)) (or op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ((eq? '&& (operator expr)) (and op1value (eval-expression (operand2 expr) environment return break continue throw)))
      ;please change
      ((eq? 'funcall (operator expr)) (interpret-funcall (cdr expr) environment return break continue throw))
      ((eq? 'dot (operator expr)) (interpret-dot (cadr expr) (caddr expr) environment return break continue throw))
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
      ((state_empty environment) #f)
      ((list? (variables (topframe environment))) (exists-in-list? var (variables (topframe environment))) #t)
      ((if_variable_there var (topframe environment)) #t)
      (else (exists? var (remainingframes environment))))))

;Checks for empty state on list of states like '( ( () ()) (() ()))
(define state_empty
  (lambda (S)
    (cond
      ((null? S) #t)
      ((not (null? (firstlayer S))) #f)
      ((and (null? (firstlayer S)) (null? (restlayer S))) #t)
      (else (state_empty (restlayer S)))))) 

(define if_variable_there
  (lambda (var S)
    (cond
      ((null? S) #f)
      ((null? (firstlayer S)) #f)
      ((eq? (firstlayer S) var) #t)
      (else (if_variable_there var (restlayer S) )))))

; Abstractions
(define firstlayer (lambda (statement) (car statement)))
(define restlayer (lambda (statement) (cdr statement)))


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
      ((zero? n) (unbox (car l)))
      (else (get-value (- n 1) (cdr l))))))

; Adds a new variable/value binding pair into the environment.  Gives an error if the variable already exists in this frame.
(define insert
  (lambda (var val environment)
    (if (exists-in-list? var (variables (firstlayer environment)))
        (myerror "error: variable is being re-declared:" var)
        (cons (add-to-frame var val (firstlayer environment)) (restlayer environment)))))

; Changes the binding of a variable to a new value in the environment.  Gives an error if the variable does not exist.
(define update
  (lambda (var val environment)
    (if (exists? var environment)
        (update-existing var val environment)
        (myerror "error: variable used but not defined:" var))))

; Add a new variable/value pair to the frame.
(define add-to-frame
  (lambda (var val frame)
    (list (cons var (variables frame)) (cons (box (scheme->language val)) (store frame))))) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BAD BOX

; Changes the binding of a variable in the environment to a new value
(define update-existing
  (lambda (var val environment)
    (cond
    ((null? environment) (myerror "Error: variable not in scope -" var ))
    (else (exists-in-list? var (variables (car environment)))
        (cons (update-in-frame var val (topframe environment)) (remainingframes environment))
        (cons (topframe environment) (update-existing var val (remainingframes environment)))))))

; Changes the binding of a variable in the frame to a new value.
(define update-in-frame
  (lambda (var val frame)
    (list (variables frame) (update-in-frame-store var val (variables frame) (store frame)))))

; Changes a variable binding by placing the new value in the appropriate place in the store
(define update-in-frame-store
  (lambda (var val varlist vallist)
    (cond
      ((eq? var (car varlist)) (cons (box (scheme->language val)) (cdr vallist))) ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BAD BOX
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

(trace interpret-main)
(trace evaluate-main-class)
(trace interpret-declare)
(trace find-function-in-closure)
(trace make-statelayer-from-instance-fields)
(trace push-frame)
(trace create-class)
(trace closure)
(trace lookup)
(trace interpret-new-object)
(trace interpret-statement-list)
(trace interpret-statement)

(trace evaluate-main-class)
    
;(parser "test1.java")
;(interpret "test1.java" "List")
;(parser "basic.java")
;(interpret "basic.java" 'B)

(parser "test1.java")
(interpret "test1.java" 'A) ;15
