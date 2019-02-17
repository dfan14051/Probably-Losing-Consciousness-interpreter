#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(require
    "parsing/simpleParser.rkt"
    "interpreting/interpreter.rkt")

(parser "examples/test.txt")
;(interpret "examples/test.txt")