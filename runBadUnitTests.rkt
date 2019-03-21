#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "parsing/functionParser.rkt"
    "interpreting/interpreter.rkt")

(parser "unit_tests/unit_test_06.txt")
(interpret "unit_tests/unit_test_06.txt")