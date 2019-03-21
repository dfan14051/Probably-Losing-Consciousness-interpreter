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
    "executor.rkt")

(provide
    interpret)

;; Given a file path, interprets and executes it.
(define interpret
    ;; param filePath is the name of the file to interpret
    (lambda (filePath)
        (call/cc
            (lambda (k)
                (execute-parse-tree
                    (parser filePath)
                    (create-state)
                    (lambda (value state)
                        (k value))
                    (lambda (exception state)
                        (error
                            "Uncaught exception"
                            exception)))))))