#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(provide (all-defined-out))

(define execute-and
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the operation
    ; param rightSide The right side of the operation
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
          (if (and (eq? leftOperand 'true) (eq? rightOperand 'true))
            'true
            'false)
          (cons newState '())))))

(define execute-or
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param leftSide The left side of the operation
    ; param rightSide The right side of the operation
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
          (if (or (eq? leftOperand 'true) (eq? rightOperand 'true))
            'true
            'false)
          (cons newState '())))))
 
(define execute-not
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param inputPhrase The boolean phrase to be inverted
    ; param state The state to use
  (lambda (execute-parse-tree inputPhrase state)
    (let*
        ([operandResult
           (execute-parse-tree inputPhrase state)]
         [operand
           (if (list? operandResult)
               (car operandResult)
               operandResult)]
         [newState
           (if (list? operandResult)
               (cadr operandResult)
               state)])
         (cons
           (cond 
             [(eq? operand 'true) 'false]
             [(eq? operand 'false) 'true]
             [else
               (error
                 "TypeError"
                 (format "~a is not a boolean" operand))])
           (cons newState '())))))

