#lang racket
;;;; ***************************************************
;;;;   Group 36 (formerly group 40)
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt"
    "evaluator.rkt"
    "stateUpdater.rkt")

(provide
    load-global-state-from-parse-tree)

(define load-global-state-from-parse-tree
    (lambda (parseTree state)
        (if (null? parseTree)
            state 
            (load-global-state-from-parse-tree
                (cdr parseTree)
                (update-state-from-parse-tree
                    (car parseTree)
                    state)))))