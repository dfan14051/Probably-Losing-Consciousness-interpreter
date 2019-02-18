#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require 
    "stateOperations.rkt")

(provide (all-defined-out))

(define execute-if
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param condition The condition to be evaluated by the conditional
    ; param state The state to use
     (lambda (execute-parse-tree parseTree state)
        (let*
            ([conditionResult
                (execute-parse-tree (cons (cadr parseTree) '()) state)]
            [conditionValue
                (if (pair? conditionResult)
                    (car conditionResult)
                    conditionResult)]
            [postConditionState
                (if (pair? conditionResult)
                    (cadr conditionResult)
                    state)])
            (cond
                [(eq? conditionValue 'true)
                    (execute-parse-tree (cons (caddr parseTree) '()) postConditionState)]
                [(null? (cdddr parseTree))
                    state]
                [else
                    (execute-parse-tree (cons (cadddr parseTree) '()) postConditionState)])
    )))
     
(define execute-while
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param condition The condition to be evaluated by the conditional
    ; param state The state to use
     (lambda (execute-parse-tree parseTree state)
        (let*
            ([conditionResult
                (execute-parse-tree (cons (cadr parseTree) '()) state)]
            [conditionValue
                (if (pair? conditionResult)
                    (car conditionResult)
                    conditionResult)]
            [postConditionState
                (if (pair? conditionResult)
                    (cadr conditionResult)
                    state)])
            (cond
                [(eq? conditionValue 'true)
                    (execute-while
                      execute-parse-tree
                      parseTree
                      (state-value (execute-parse-tree (cons (caddr parseTree) '()) postConditionState)))]
                [else postConditionState]))))

;; Returns the value of the state
(define state-value
    ; param value The value to return
    (lambda (value)
        (if (and (pair? value) (not (pair? (car value))))
            (cadr value)
            value)))
