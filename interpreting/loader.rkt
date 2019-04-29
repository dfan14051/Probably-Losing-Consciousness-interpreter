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
    "stateUpdater.rkt"
    "functions.rkt")

(provide
    load-global-state-from-parse-tree)

(define load-global-state-from-parse-tree
    (lambda (parseTree state execute-parse-tree throw)
        (if (null? parseTree)
            state
            (load-global-state-from-parse-tree
                (cdr parseTree)
                (add-class-closure-to-state
                    (car parseTree)
                    state)
                execute-parse-tree
                throw))))

;;;; HELPER FUNCTIONS
(define instance_functions_atom '__instance_functions)
(define class_name_atom '__class_name)
(define super_class_name_atom '__super_class_name)
(define constructor_atom 'new)

(define get-class-name cadr)
(define get-super-class-name
    (lambda (classParseTree)
        (if (null? (caddr classParseTree))
            '()
            (cadr (caddr classParseTree)))))
(define get-class-body cadddr)

(define add-class-closure-to-state
    (lambda (classParseTree state)
        (set-var-value
            (get-class-name classParseTree)
            (get-class-closure-from-class-body
                (get-class-body classParseTree)
                state
                (get-class-name classParseTree)
                (get-super-class-name classParseTree))
            (add-var-to-state
                (get-class-name classParseTree)
                state))))

(define get-class-closure-from-class-body
    (lambda (classBodyParseTree state className superClassName)
        (get-functions-and-statics-from-class-body
            classBodyParseTree
            state
            (create-initial-class-closure classBodyParseTree state className superClassName))))

(define create-initial-class-closure
    (lambda (classBodyParseTree state className superClassName)
        (set-var-value
            instance_functions_atom
            '()
            (add-var-to-state
                instance_functions_atom
                (set-var-value
                    super_class_name_atom
                    superClassName
                    (add-var-to-state
                        super_class_name_atom
                        (set-var-value
                            constructor_atom
                            (create-constructor-data
                                classBodyParseTree
                                state
                                className
                                superClassName)
                            (add-var-to-state
                                constructor_atom
                                (create-state)))))))))

(define get-function-command car)
(define get-function-name cadr)
(define get-param-list caddr)
(define get-function-body cadddr)

(define get-var-command car)
(define get-var-name cadr)
(define get-var-expr caddr)

(define get-functions-and-statics-from-class-body
    (lambda (classBodyParseTree state closure)
        (cond
            [(null? classBodyParseTree)
                closure]
            [(eq? 'static-function (caar classBodyParseTree))
                (get-functions-and-statics-from-class-body
                    (cdr classBodyParseTree)
                    state
                    (set-var-value
                        (get-function-name (get-function-command classBodyParseTree))
                        (create-function-data
                            (get-function-command classBodyParseTree)
                            state
                            #t)
                        (add-var-to-state
                            (get-function-name (get-function-command classBodyParseTree))
                            closure)))]
            [(eq? 'static-var (caar classBodyParseTree))
                (get-functions-and-statics-from-class-body
                    (cdr classBodyParseTree)
                    state
                    (set-var-value
                        (get-var-name (get-var-command classBodyParseTree))
                        (get-var-expr (get-var-command classBodyParseTree))
                        (add-var-to-state
                            (get-var-name (get-var-command classBodyParseTree))
                            closure)))]
            [(eq? 'function (caar classBodyParseTree))
                (get-functions-and-statics-from-class-body
                    (cdr classBodyParseTree)
                    state
                    (set-var-value
                        instance_functions_atom
                        (cons
                            (list
                                (get-function-name (get-function-command classBodyParseTree))
                                (get-function-command classBodyParseTree))
                            (get-var-value
                                instance_functions_atom
                                closure))
                        closure))]
            [else
                (get-functions-and-statics-from-class-body
                    (cdr classBodyParseTree)
                    state
                    closure)])))

(define create-constructor-data
    (lambda (classBodyParseTree state className superClassName)
        (lambda ()
            (execute-class-body
                classBodyParseTree
                (if (null? superClassName)
                    (push-instance-scope state className)
                    (push-instance-scope
                        (evaluate-parse-tree
                            (list 'new superClassName)
                            state
                            update-state-from-parse-tree
                            update-state-from-command-list
                            execute-parse-tree
                            constructor-throw)
                        className))))))

(define push-instance-scope
    (lambda (state className)
        (set-var-value
            class_name_atom
            className
            (add-var-to-state
                class_name_atom
                (push-empty-scope state)))))

(define constructor-throw
    (lambda (exception)
        (error
            "Exception occurred in constructor"
            exception)))

(define execute-class-body ; Used exclusively for constructors
    (lambda (remainingClassBodyParseTree objectState)
        ;;; (displayln 'EXECUTING-CLASS-BODY)
        ;;; (displayln objectState)
        (if (null? remainingClassBodyParseTree)
            objectState
            (execute-class-body
                (cdr remainingClassBodyParseTree)
                (update-state-from-parse-tree
                    (car remainingClassBodyParseTree)
                    objectState
                    execute-parse-tree
                    constructor-throw)))))
