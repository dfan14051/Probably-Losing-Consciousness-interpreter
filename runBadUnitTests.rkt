#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "parsing/classParser.rkt"
    "interpreting/interpreter.rkt")

(parser "unit_tests/part4/unit_test_04.txt")
(interpret "unit_tests/part4/unit_test_04.txt" 'A)

;(parser "unit_tests/part4/unit_test_12.txt")
;(interpret "unit_tests/part4/unit_test_12.txt" 'List)

;(parser "unit_tests/part4/unit_test_13.txt")
;(interpret "unit_tests/part4/unit_test_13.txt" 'List)