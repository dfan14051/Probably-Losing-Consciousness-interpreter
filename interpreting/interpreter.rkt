#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "../parsing/simpleParser.rkt"
    "stateOperations.rkt"
    "executor.rkt")

(provide
    interpret)

;; Given a file path, interprets and executes it.
(define interpret
    ;; param filePath is the name of the file to interpret
    (lambda (filePath)
        (execute-parse-tree (parser filePath) (create-state))))