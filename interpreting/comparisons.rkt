#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(provide (all-defined-out))

(define execute-equality
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the equality
    ; param rightSide The right side of the equality
    ; param state The state to use
  (lambda (execute-parse-tree leftSide rightSide state)
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
        (cons
          (if (eq? leftOperand rightOperand)
            'true
            'false)
          (cons newState '())))))
          
(define execute-inequality
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the comparison
    ; param rightSide The right side of the comparison
    ; param state The state to use
  (lambda (execute-parse-tree leftSide rightSide state)
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
        (cons
          (if (eq? leftOperand rightOperand)
            'false
            'true)
          (cons newState '())))))         

(define execute-lessthan
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the comparison
    ; param rightSide The right side of the comparison
    ; param state The state to use
  (lambda (execute-parse-tree leftSide rightSide state)
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
        (cons
          (if (< leftOperand rightOperand)
            'true
            'false)
          (cons newState '())))))         

(define execute-greaterthan
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the comparison
    ; param rightSide The right side of the comparison
    ; param state The state to use
  (lambda (execute-parse-tree leftSide rightSide state)
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
        (cons
          (if (> leftOperand rightOperand)
            'true
            'false)
          (cons newState '())))))         

(define execute-lessthanoreq
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the comparison
    ; param rightSide The right side of the comparison
    ; param state The state to use
  (lambda (execute-parse-tree leftSide rightSide state)
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
        (cons
          (if (<= leftOperand rightOperand)
            'true
            'false)
          (cons newState '())))))         

(define execute-greaterthanoreq
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the comparison
    ; param rightSide The right side of the comparison
    ; param state The state to use
  (lambda (execute-parse-tree leftSide rightSide state)
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
        (cons
          (if (>= leftOperand rightOperand)
            'true
            'false)
          (cons newState '())))))         

