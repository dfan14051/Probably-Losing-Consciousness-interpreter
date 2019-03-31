#lang racket
;;;; ***************************************************
;;;;   Group 36 (formerly group 40)
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "stateOperations.rkt")

(provide
    create-function-data
    leave-function-environment)

;; Creates the function data required to add a function to the state
(define create-function-data
    ;; param command The command that declares the function
    (lambda (command outerEnvironment)
        ((lambda (paramList bodyParseTree)
            (list
                paramList
                bodyParseTree
                (lambda (argList state)
                    ;(displayln outerEnvironment)
                    (add-args-to-scope
                        paramList
                        argList
                        (push-scope outerEnvironment)))))
            (caddr command)
            (cadddr command))))

(define leave-function-environment pop-scope)

;;;; HELPER FUNCTIONS
(define add-args-to-scope
    (lambda (paramList argList state)
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