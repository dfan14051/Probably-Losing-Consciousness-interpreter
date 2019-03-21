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

(parser "examples/functions_example.txt")
(interpret "examples/functions_example.txt")
