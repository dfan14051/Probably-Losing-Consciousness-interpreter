#lang racket
;;;; ***************************************************
;;;;   Group 36 (formerly group 40)
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "functions.rkt"
    "stateOperations.rkt"
    "evaluator.rkt")

(provide
    update-state-from-parse-tree
    update-state-from-command-list)

;; Executes a statement and returns the resulting state
(define update-state-from-parse-tree
    ; param statement The parse tree to execute
    ; param state The state to use
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to use in the case of a throw in a function call
    (lambda (statement state execute-parse-tree throw)
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
                (update-state-from-var-init
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '= (car statement))
                ;; Assigning a value to a variable
                ;; Continue onwards with the new state
                (update-state-from-var-assign
                    statement
                    state
                    execute-parse-tree
                    throw)]

            ;;;; FUNCTION DECLARATION
            [(eq? 'function (car statement))
                ;; Creating a new variable
                ;; Continue onwards with the new state
                (update-state-from-function-declaration statement state)]

            ;;;; FUNCTION CALL
            [(eq? 'funcall (car statement))
                ((lambda (funcData)
                    (call/cc (lambda (k)
                        (pop-scope (cadr (execute-parse-tree
                            (cadr funcData)
                            ((caddr funcData)
                                (evaluate-list-of-values ; the arg list
                                    (cddr statement)
                                    state
                                    update-state-from-parse-tree
                                    update-state-from-command-list
                                    execute-parse-tree
                                    throw)
                                (update-state-from-command-list
                                    (cddr statement)
                                    state
                                    execute-parse-tree
                                    throw))
                            (lambda (v s)
                                (k (leave-function-environment s)))
                            throw))))))
                    (get-var-value (cadr statement) state))]

            ;;;; VALUE OPERATIONS THAT CAN AFFECT STATE
            [(eq? '* (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '/ (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '% (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '+ (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '- (car statement))
                (if (null? (cddr statement))
                    (update-state-from-unary-operation-side
                        statement
                        state
                        execute-parse-tree
                        throw)
                    (update-state-from-binary-operation-sides
                        statement
                        state
                        execute-parse-tree
                        throw))]
            [(eq? '== (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '!= (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '< (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '> (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '<= (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '>= (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '&& (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '|| (car statement))
                (update-state-from-binary-operation-sides
                    statement
                    state
                    execute-parse-tree
                    throw)]
            [(eq? '! (car statement))
                (update-state-from-unary-operation-side
                    statement
                    state
                    execute-parse-tree
                    throw)]

            ;;;; FALLBACK
            [else
                ;; Something was unrecognized
                ;; This could mean it's a different type of action
                ;; Just move on
                state])))
            
;; Executes a list of subsequent commands and returns the resulting state
(define update-state-from-command-list
    ; param commands The commands to execute
    ; param state The state to use
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to use in the case of a throw in a function call
    (lambda (commands state execute-parse-tree throw)
        (if (null? commands)
            state
            (update-state-from-command-list
                (cdr commands)
                (update-state-from-parse-tree
                    (car commands)
                    state
                    execute-parse-tree
                    throw)
                execute-parse-tree
                throw))))

;;;; HELPER FUNCTIONS
;; Executes a variable initialization and returns the new state
(define update-state-from-var-init
    ; param command The command to execute
    ; param state The current state
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to use in the case of a throw in a function call
    (lambda (command state execute-parse-tree throw)
        (if (null? (cddr command))
            ;; No value being assigned
            (add-var-to-state 
                (cadr command)
                state)
            ;; A value being assigned at initialization
            (set-var-value
                (cadr command)
                (evaluate-parse-tree
                    (caddr command)
                    state
                    update-state-from-parse-tree
                    update-state-from-command-list
                    execute-parse-tree
                    throw)
                (add-var-to-state
                    (cadr command)
                    (update-state-from-parse-tree
                        (caddr command)
                        state
                        execute-parse-tree
                        throw))))))

;; Assigns a value to a variable and returns the new state
(define update-state-from-var-assign
    ; param command The command to execute
    ; param state The current state
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to use in the case of a throw in a function call
    (lambda (command state execute-parse-tree throw)
        (set-var-value
            (cadr command)
            (evaluate-parse-tree
                (caddr command)
                state
                update-state-from-parse-tree
                update-state-from-command-list
                execute-parse-tree
                throw)
            (update-state-from-parse-tree
                (caddr command)
                state
                execute-parse-tree
                throw))))

;; Assigns a function body to a name in the new state
(define update-state-from-function-declaration
    ; param command The command to execute
    ; param state The current state
    (lambda (command state)
        ((lambda (funcName)
            (set-var-value
                funcName
                (create-function-data command)
                (add-var-to-state
                    funcName
                    state)))
            (cadr command))))

(define update-state-from-unary-operation-side
    ; param command The command to execute
    ; param state The current state
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to use in the case of a throw in a function call
    (lambda (command state execute-parse-tree throw)
        (update-state-from-parse-tree
            (cadr command)
            state
            execute-parse-tree
            throw)))

(define update-state-from-binary-operation-sides
    ; param command The command to execute
    ; param state The current state
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to use in the case of a throw in a function call
    (lambda (command state execute-parse-tree throw)
        (update-state-from-parse-tree 
            (caddr command)
            (update-state-from-parse-tree
                (cadr command)
                state
                execute-parse-tree
                throw)
            execute-parse-tree
            throw)))