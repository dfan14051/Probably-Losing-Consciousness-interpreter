#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require 
    "stateOperations.rkt")

(provide (all-defined-out))

(define update-state-from-if
    ; param update-state-from-parse-tree The execution function to call to evaluate the left and right sides
    ; param condition The condition to be evaluated by the conditional
    ; param state The state to use
     (lambda (update-state-from-parse-tree parseTree state)
        (let*
            ([conditionResult
                (update-state-from-parse-tree (cons (cadr parseTree) '()) state)]
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
                    (update-state-from-parse-tree (cons (caddr parseTree) '()) postConditionState)]
                [(null? (cdddr parseTree))
                    state]
                [else
                    (update-state-from-parse-tree (cons (cadddr parseTree) '()) postConditionState)])
    )))
     
(define update-state-from-while
    ; param update-state-from-parse-tree The execution function to call to evaluate the left and right sides
    ; param condition The condition to be evaluated by the conditional
    ; param state The state to use
     (lambda (update-state-from-parse-tree parseTree state)
        (let*
            ([conditionResult
                (update-state-from-parse-tree (cons (cadr parseTree) '()) state)]
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
                    (update-state-from-while
                      update-state-from-parse-tree
                      parseTree
                      (state-value (update-state-from-parse-tree (cons (caddr parseTree) '()) postConditionState)))]
                [else postConditionState]))))
