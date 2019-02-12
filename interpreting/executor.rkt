#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt"
    "arithmetic.rkt")

(provide
    execute-parse-tree)

;; Executes a list of commands
(define execute-parse-tree
    ; param parseTree The parse tree to execute
    ; param state The state to use
    (lambda (parseTree state)
        (cond
            ;;;; BASE CASES
            [(null? parseTree)
                ;; Don't do anything
                '()]
            [(null? (car parseTree))
                ;; The next element is an empty list
                ;; So just move on
                (execute-parse-tree (cdr parseTree))]
            [(not (pair? (car parseTree)))
                ;; The next element is not a pair, so it's an atom
                ;; It must be a single value
                ;; So return that value and the current state
                (get-atom-value (car parseTree) state)]

            ;;;; ARITHMETIC
            [(eq? '* (caar parseTree))
                (execute-arithmetic execute-parse-tree * (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '/ (caar parseTree))
                (execute-arithmetic execute-parse-tree quotient (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '% (caar parseTree))
                (execute-arithmetic execute-parse-tree remainder (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '+ (caar parseTree))
                (execute-arithmetic execute-parse-tree + (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '- (caar parseTree))
                (if (null? (cddar parseTree))
                    (cons (* -1 (car (execute-parse-tree (cdar parseTree) state))) (cons state '()))
                    (execute-arithmetic execute-parse-tree - (get-left-side parseTree) (get-right-side parseTree) state))]

            ;;;; STATEMENTS
            [(eq? 'var (caar parseTree))
                ;; Creating a new variable
                ;; Continue onwards with the new state
                (execute-parse-tree
                    (cdr parseTree)
                    (execute-var-init (car parseTree) state))]
            [(eq? '= (caar parseTree))
                ;; Assigning a value to a variable
                ;; Continue onwards with the new state
                (let*
                    ([result
                        (execute-var-assign (car parseTree) state)]
                    [newState
                        (cadr result)])
                    (if (null? (cdr parseTree))
                        result
                        (execute-parse-tree (cdr parseTree) newState)))]
            [(eq? 'return (caar parseTree))
                ;; A return statement
                ;; Just return the result of this statement
                (car (execute-parse-tree (cdar parseTree) state))]

            ;;;; FALLBACK
            [else
                ;; Something went wrong...
                (error
                    "unknown parseTree element"
                    (format "Did not recognize operation ~a" (caar parseTree)))])))

;; Executes a variable initialization and returns the new state
(define execute-var-init
    ; param command The command to execute
    ; param state The current state
    (lambda (command state)
        (if (null? (cddr command))
            ;; No value being assigned
            (add-var-to-state 
                (cadr command)
                state)
            ;; A value being assigned at initialization
            (set-var-value
                (cadr command)
                (execute-parse-tree (cddr command) state)
                (add-var-to-state (cadr command) state)))))

;; Assigns a value to a variable and returns the new state
(define execute-var-assign
    ; param command The command to execute
    ; param state The current state
    (lambda (command state)
        (let*
            ([result
                (execute-parse-tree (cddr command) state)]
            [varValue
                (if (pair? result) (car result) result)]
            [resultState
                (if (pair? result) (cadr result) state)]
            [newState
                (set-var-value (cadr command) varValue resultState)])
            (cons varValue (cons newState '())))))

;; Gets the value of a single atom
(define get-atom-value
    ; param a The atom to evaluate
    ; param state The current state
    (lambda (a state)
        (if (number? a)
            a
            (get-var-value a state))))