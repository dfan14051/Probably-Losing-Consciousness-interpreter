#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "stateOperations.rkt")

(provide
    create-function-data)

;; Creates the function data required to add a function to the state
(define create-function-data
    ;; param command The command that declares the function
    ;; param globalScope The global scope box
    ;; param objectScopeList The list of scope boxes for the object
    ;; param isStatic Whether the function is static
    (lambda (command globalScope objectScopeList isStatic)
        ((lambda (paramList bodyParseTree)
            (list
                paramList
                bodyParseTree
                (lambda (argList state)
                    (add-args-to-scope
                        ; Possibly add this and super to the paramList
                        (if isStatic
                            paramList
                            (cons (list 'this 'super) paramList))
                        ; Possibly add this and super to the argList
                        (if isStatic
                            argList
                            (cons (list (list (car objectScopeList)) (list (cadr objectScopeList))) argList))
                        (push-scope (append objectScopeList (list globalScope)))))))
            (caddr command)
            (cadddr command))))

;;;; HELPER FUNCTIONS
(define add-args-to-scope
    (lambda (paramList argList shouldAddThis state)
        (if (null? paramList)
            state
            (add-args-to-scope
                (cdr paramList)
                (cdr argList)
                (set-var-value
                    (car paramList)
                    (car argList)
                    (add-var-to-state
                        (car paramList)
                        state))))))