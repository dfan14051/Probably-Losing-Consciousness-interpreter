#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt"
    "evaluator.rkt")

(provide
    update-state-from-parse-tree)

;; Executes a list of commands
(define update-state-from-parse-tree
    ; param statement The parse tree to execute
    ; param state The state to use
    (lambda (statement state)
        (cond
            ;;;; BASE CASES
            [(null? statement)
                ;; Don't do anything
                state]
            [(not (pair? statement))
                ;; It's not a pair so it can't affect the state
                state]
            [(null? (cdr statement))
                ;; It must be a single atom
                ;; That can't affect the state, so move on
                state]

            ;;;; STATEMENTS
            [(eq? 'var (car statement))
                ;; Creating a new variable
                ;; Continue onwards with the new state
                (update-state-from-var-init statement state)]
            [(eq? '= (car statement))
                ;; Assigning a value to a variable
                ;; Continue onwards with the new state
                (update-state-from-var-assign statement state)]

            ;;;; VALUE OPERATIONS THAT CAN AFFECT STATE
            [(eq? '* (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '/ (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '% (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '+ (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '- (car statement))
                (if (null? (cddr statement))
                    (update-state-from-unary-operation-side statement state)
                    (update-state-from-binary-operation-sides statement state))]
            [(eq? '== (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '!= (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '< (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '> (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '<= (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '>= (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '&& (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '|| (car statement))
                (update-state-from-binary-operation-sides statement state)]
            [(eq? '! (car statement))
                (update-state-from-unary-operation-side statement state)]

            ;;;; FALLBACK
            [else
                ;; Something was unrecognized
                ;; This could mean it's a different type of action
                ;; Just move on
                state])))

;;;; HELPER FUNCTIONS

;; Executes a variable initialization and returns the new state
(define update-state-from-var-init
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
                (evaluate-parse-tree
                    (caddr command) state
                    update-state-from-parse-tree)
                (add-var-to-state
                    (cadr command)
                    (update-state-from-parse-tree (caddr command) state))))))

;; Assigns a value to a variable and returns the new state
(define update-state-from-var-assign
    ; param command The command to execute
    ; param state The current state
    (lambda (command state)
        (set-var-value
            (cadr command)
            (evaluate-parse-tree
                (caddr command) state
                update-state-from-parse-tree)
            (update-state-from-parse-tree (caddr command) state))))

(define update-state-from-unary-operation-side
    ; param command The command to execute
    ; param state The current state
    (lambda (command state)
        (update-state-from-parse-tree (cadr command) state)))

(define update-state-from-binary-operation-sides
    ; param command The command to execute
    ; param state The current state
    (lambda (command state)
        (update-state-from-parse-tree 
            (caddr command)
            (update-state-from-parse-tree (cadr command) state))))