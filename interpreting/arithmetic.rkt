#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(provide
    execute-arithmetic
    get-left-side
    get-right-side)

;; Gets the left-hand-side for an arithmetic operation
(define get-left-side
    ; param parseTree The parse three to extract the LHS from
    (lambda (parseTree)
        (cons (cadar parseTree) '())))

;; Gets the right-hand-side for an arithmetic operation
(define get-right-side
    ; param parseTree The parse three to extract the RHS from
    (lambda (parseTree)
        (cons (caddar parseTree) '())))

;; Executes an arithmetic operation, returns (value, state)
(define execute-arithmetic
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param operator The arithmetic operator to use
    ; param leftSide The left side of the operator
    ; param rightSide The right side of the operator
    ; param state The state to use
    (lambda (execute-parse-tree operator leftSide rightSide state)
        (let*
            ([leftOperandResult
                (execute-parse-tree leftSide state)]
            [leftOperand
                (if (pair? leftOperandResult)
                    (car leftOperandResult)
                    leftOperandResult)]
            [rightOperandResult
                (execute-parse-tree rightSide (if (pair? leftOperandResult) (cadr leftOperandResult) state))]
            [rightOperand
                (if (pair? rightOperandResult)
                    (car rightOperandResult)
                    rightOperandResult)]
            [newState
                (if (pair? rightOperandResult)
                    (cadr rightOperandResult)
                    (if (pair? leftOperandResult)
                        (cadr leftOperandResult)
                        state))])
            (cons (operator leftOperand rightOperand) (cons newState '())))))