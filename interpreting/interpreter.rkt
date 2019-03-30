#lang racket
;;;; ***************************************************
;;;;   Group 36 (formerly group 40)
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "../parsing/functionParser.rkt"
    "stateOperations.rkt"
    "executor.rkt"
    "loader.rkt")

(provide
    interpret)

;; Given a file path, interprets and executes it.
(define interpret
    ;; param filePath is the name of the file to interpret
    (lambda (filePath)
        ((lambda (parseTree)
            (call/cc
                (lambda (k)
                    (execute-parse-tree
                        '((return (funcall main)))
                        (load-global-state-from-parse-tree
                            parseTree
                            (create-state)
                            execute-parse-tree
                            base-throw)
                        (lambda (value state)
                            (k value))
                        base-throw
                        ))))
            (parser filePath))))

;;;; Helper Functions
(define base-throw
    (lambda (exception state)
                            (error
                                "Uncaught exception"
                                exception)))