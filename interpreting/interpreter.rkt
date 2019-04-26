#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "../parsing/classParser.rkt"
    "stateOperations.rkt"
    "executor.rkt"
    "loader.rkt")

(provide
    interpret)

;; Given a file path, interprets and executes it.
(define interpret
    ;; param filePath The name of the file to interpret
    ;; param mainClassName The name of the class that contains the main method
    (lambda (filePath mainClassName)
        ((lambda (parseTree)
            (call/cc
                (lambda (k)
                    (execute-parse-tree
                        '((return (funcall (dot mainClassName main))))
                        (push-scope 
                            (load-global-state-from-parse-tree
                                parseTree
                                (create-state)
                                execute-parse-tree
                                base-throw
                            )
                        )
                        (lambda (value)
                            (k value))
                        base-throw
                        ))))
            (parser filePath))))

;;;; Helper Functions
(define base-throw
    (lambda (exception)
        (error
            "Uncaught exception"
            exception)))