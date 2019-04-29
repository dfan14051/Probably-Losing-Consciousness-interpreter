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
    ;; param outerFunctionEnvironment The outer function environment
    ;; param isStatic Whether the function is static
    (lambda (command outerFunctionEnvironment isStatic)
        ((lambda (paramList bodyParseTree this super)
            (list
                paramList
                bodyParseTree
                (lambda (argList functionEnvironment)
                    (add-args-to-scope
                        ; Possibly add this and super to the paramList
                        (if isStatic
                            paramList
                            (cons 'this (cons 'super paramList)))
                        ; Possibly add this and super to the argList
                        (if isStatic
                            argList
                            (cons this (cons super argList)))
                        (push-empty-scope outerFunctionEnvironment)))))
            (get-param-list command)
            (get-function-body command)
            (create-this outerFunctionEnvironment)
            (create-super outerFunctionEnvironment))))

;;;; HELPER FUNCTIONS
(define get-function-name cadr)
(define get-param-list caddr)
(define get-function-body cadddr)

(define create-this
    (lambda (outerFunctionEnvironment)
        (if (null? outerFunctionEnvironment)
            '()
            outerFunctionEnvironment)))

(define create-super
    (lambda (outerFunctionEnvironment)
        (if (or (null? outerFunctionEnvironment) (or (null? (cdr outerFunctionEnvironment)) (null? (cddr outerFunctionEnvironment))))
            '()
            ((lambda (instance-functions)
                (push-scope
                    (car instance-functions)
                    (cadr instance-functions)
                    (pop-scope outerFunctionEnvironment)))
                (get-instance-functions-from-outer-function-environment
                    (pop-scope outerFunctionEnvironment))))))

(define instance_functions_atom '__instance_functions)
(define class_name_atom '__class_name)
(define get-instance-functions-from-outer-function-environment
    (lambda (outerFunctionEnvironment)
        (get-instance-functions-from-list
            '()
            '()
            (get-var-value
                instance_functions_atom
                (get-var-value
                    (get-var-value class_name_atom outerFunctionEnvironment)
                    outerFunctionEnvironment))
            outerFunctionEnvironment)))

(define get-instance-functions-from-list
    (lambda (functionNames functionValues l outerFunctionEnvironment)
        (if (null? l)
            (list functionNames functionValues)
            (get-instance-functions-from-list
                (cons (caar l) functionNames)
                (cons
                    (create-function-data (cadar l) outerFunctionEnvironment #f)
                    functionValues)
                (cdr l)
                outerFunctionEnvironment))))

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