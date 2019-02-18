#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt"
    "arithmetic.rkt"
    "comparisons.rkt"
    "booleanops.rkt"
    "conditionals.rkt")

(provide
    execute-parse-tree)

;; Executes a list of commands
(define execute-parse-tree
    ; param parseTree The parse tree to execute
    ; param state The state to use
    (lambda (parseTree state)
        (cond
            ;;;; BASE CASES
            [(not (pair? state))
                state]
            [(null? parseTree)
                ;; Don't do anything
                state]
            [(not (pair? parseTree))
                ;; The parseTree must just be an atom
                (get-atom-value parseTree state)]
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

            ;;;; COMPARISONS
            [(eq? '== (caar parseTree))
                (execute-equality execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '!= (caar parseTree))
                (execute-inequality execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '< (caar parseTree))
                (execute-lessthan execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '> (caar parseTree))
                (execute-greaterthan execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '<= (caar parseTree))
                (execute-lessthanoreq execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '>= (caar parseTree))
                (execute-greaterthanoreq execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]

            ;;;; BOOLEAN OPERATORS
            [(eq? '&& (caar parseTree))
                (execute-and execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '|| (caar parseTree))
                (execute-or execute-parse-tree (get-left-side parseTree) (get-right-side parseTree) state)]
            [(eq? '! (caar parseTree))
                (execute-not execute-parse-tree (cdar parseTree) state)]

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
                (return-value (execute-parse-tree (cdar parseTree) state))]
            [(eq? 'if (caar parseTree))
                ;; An if statement
                ;; Evaluate what comes after for boolean value.
                (execute-parse-tree 
                    (cdr parseTree)
                    (state-value (execute-if execute-parse-tree (car parseTree) state)))]
            [(eq? 'while (caar parseTree))
                ;; A while loop
                (execute-parse-tree
                    (cdr parseTree)
                    (state-value (execute-while execute-parse-tree (car parseTree) state)))]

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
                (return-value (execute-parse-tree (cddr command) state))
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
        (cond [(number? a) a]
          [(eq? a 'true) a]
          [(eq? a 'false) a]
          [else (get-var-value a state)])))