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
                (update-state-from-parse-tree
                    (car parseTree)
                    state
                    execute-parse-tree
                    throw)
                execute-parse-tree
                throw))))

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