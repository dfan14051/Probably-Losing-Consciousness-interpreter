#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(provide
    execute-arithmetic
    get-left-side
    get-right-side)

;; Gets the left-hand-side for an arithmetic operation
(define get-left-side
    (lambda (parseTree)
        (cons (cadar parseTree) '())))

;; Gets the right-hand-side for an arithmetic operation
(define get-right-side
    (lambda (parseTree)
        (cons (caddar parseTree) '())))

;; Executes an arithmetic operation
(define execute-arithmetic
    (lambda (execute-parse-tree operator leftSide rightSide state)
        (let*
            ([leftOperandResult
                (execute-parse-tree leftSide state)]
            [leftOperand
                (if (list? leftOperandResult)
                    (car leftOperandResult)
                    leftOperandResult)]
            [rightOperandResult
                (execute-parse-tree rightSide (if (list? leftOperandResult) (cadr leftOperandResult) state))]
            [rightOperand
                (if (list? rightOperandResult)
                    (car rightOperandResult)
                    rightOperandResult)]
            [newState
                (if (list? rightOperandResult)
                    (cadr rightOperandResult)
                    (if (list? leftOperandResult)
                        (cadr leftOperandResult)
                        state))])
            (cons (operator leftOperand rightOperand) (cons newState '())))))