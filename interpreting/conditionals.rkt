#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require "stateOperations.rkt")
(provide (all-defined-out))

(define execute-if
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param condition The condition to be evaluated by the conditional
    ; param state The state to use
  (lambda (execute-parse-tree condition state)
    (let*
      ([conditionResult
         (execute-parse-tree (car condition) state)]
       [conditionValue
         (if (list? conditionResult)
             (car conditionResult)
             conditionResult)]
       [newState
         (if (list? conditionResult)
             (cadr conditionResult)
             state)]
       [result
         (if (eq? conditionValue 'true)
             (execute-parse-tree (cadr condition) (push-scope state))
             (execute-parse-tree (caddr condition) (push-scope state)))]
       [resultValue
         (if (list? result)
             (car result)
             result)]
       [resultState
         (if (list? result)
             (pop-scope (cadr result))
             state)])
       (cons resultValue (cons resultState '())))))
       
(define execute-while
    ; param execute-parse-tree The execution function to call to evaluate the left and right sides
    ; param condition The condition to be evaluated by the conditional
    ; param state The state to use
  (lambda (execute-parse-tree condition state)
 
