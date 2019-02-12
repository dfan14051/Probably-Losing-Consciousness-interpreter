#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt")

(provide
    execute-parse-tree)

;; Executes a list of commands
(define execute-parse-tree
    (lambda (parseTree state)
        (cond
            ;;;; BASE CASES
            [(null? parseTree)
                ;; Don't do anything
                '()]
            [(not (pair? parseTree))
                ;; It's not null or a pair, so it's an atom
                ;; Just return that
                (get-atom-value parseTree state)]

            ;;;; ARITHMETIC
            [(eq? '+ (car parseTree))
                (+ (execute-parse-tree (cadr parseTree) state) (execute-parse-tree (caddr parseTree) state))]
            
            ;;;; MORE BASE CASES
            [(null? (car parseTree))
                ;; The next element is an empty list
                ;; So just move on
                (execute-parse-tree (cdr parseTree))]
            [(not (pair? (car parseTree)))
                ;; The next element is not a pair, so it's an atom
                ;; It must be a single value
                (get-atom-value (car parseTree) state)]

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
                (execute-parse-tree
                    (cdr parseTree)
                    (execute-var-assign (car parseTree) state))]
            [(eq? 'return (caar parseTree))
                ;; A return statement
                ;; Just return the result of this statement
                (execute-parse-tree (cadar parseTree) state)]

            ;;;; FALLBACK
            [else
                ;; Something went wrong...
                (error
                    "unknown parseTree element"
                    (format "Did not recognize operation ~a" (caar parseTree)))])))

;; Executes a variable initialization and returns the new state
(define execute-var-init
    (lambda (command state)
        (if (null? (cddr command))
            ;; No value being assigned
            (add-var-to-state 
                (cadr command)
                state)
            ;; A value being assigned at initialization
            (set-var-value
                (cadr command)
                (execute-parse-tree (caddr command) state)
                (add-var-to-state (cadr command) state)))))

;; Assigns a value to a variable and returns the new state
(define execute-var-assign
    (lambda (command state)
        (set-var-value
            (cadr command)
            (execute-parse-tree
                (cddr command)
                state)
            state)))

;; Gets the value of a single atom
(define get-atom-value
    (lambda (a state)
        (if (number? a)
            a
            (get-var-value a state))))