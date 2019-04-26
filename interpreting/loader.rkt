#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt"
    "executor.rkt"
    "evaluator.rkt"
    "stateUpdater.rkt")

(provide
    load-global-state-from-parse-tree)

(define load-global-state-from-parse-tree
    (lambda (parseTree state execute-parse-tree throw)
        (if (null? parseTree)
            state
            (load-global-state-from-parse-tree
                (cdr parseTree)
                (load-class-state-from-parse-tree
                    (car parseTree)
                    state)
                execute-parse-tree
                throw))))

(define load-class-state-from-parse-tree
    (lambda (parseTree state)
        (set-var-value
            (cadr parseTree)
            (get-class-closure-from-class-body
                parseTree
                state)
            (add-var-to-state
                (cadr parseTree)
                state))))

(define get-class-closure-from-class
    (lambda (parseTree globalState)
        (get-main-function-from-class-body
            (cadddr parseTree)
            globalState
            (set-var-value
                'new
                (create-constructor-data
                    globalState
                    (cadddr parseTree)
                    (cadr (caddr parseTree)))
                (add-var-to-state
                    'new
                    (create-state))))))

(define get-main-function-from-class-body
    (lambda (parseTree globalState closure)
        (cond
            [(null? parseTree) closure]
            [(eq? 'static-function (caar parseTree))
                (get-main-function-from-class-body
                    (cdr parseTree)
                    globalState
                    (set-var-value
                        (cadar parseTree)
                        (create-function-data
                            (car (cdddar parseTree))
                            globalState
                            '()
                            #t)
                        (add-var-to-state
                            (cadar parseTree)
                            closure)))]
            [else
                (get-main-function-from-class-body
                    (cdr parseTree)
                    globalState
                    closure)])))
;;;; HELPER FUNCTIONS
(define create-constructor-data
    (lambda (globalScope classBodyParseTree extendsName)
        (execute-class-body
            classBodyParseTree
            (if (null? extendsName)
                (create-state)
                (push-scope (evaluate-function
                    ('dot extendsName 'new)
                    (create-state)
                    update-state-from-parse-tree
                    update-state-from-command-list
                    execute-parse-tree
                    (lambda (exception)
                        (error
                            "Exception occurred in constructor"
                            exception))))))))

(define execute-class-body ; Used exclusively for constructors
    (lambda (remainingClassBodyParseTree objectState throw)
        (if (null? remainingClassBodyParseTree))
            objectState
            (execute-class-body
                (cdr remainingClassBodyParseTree)
                (update-state-from-parse-tree
                    (car remainingClassBodyParseTree)
                    objectState
                    execute-parse-tree
                    throw))))
