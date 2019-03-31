#lang racket
;;;; ***************************************************
;;;;   Group 36 (formerly group 40)
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt")

(provide
    evaluate-parse-tree
    evaluate-list-of-values)

;; Evaluates a list of commands
;; Returns the resulting value
(define evaluate-parse-tree
    ; param command The parse tree to evaluate
    ; param state The state to use
    ; param update-state-from-parse-tree The function to call for updating state
    ; param update-state-from-command-list The function to call for updating state from a command list (like function arguments)
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to pass in the case of a function call for throw
    (lambda (command state update-state-from-parse-tree update-state-from-command-list execute-parse-tree throw)
        (cond
            ;;;; BASE CASES
            [(null? command)
                ;; Don't do anything
                '()]
            [(not (pair? command))
                ;; It must be a single atom
                (get-atom-value command state)]
            [(null? (cdr command))
                ;; It must be a single atom
                (get-atom-value (car command) state)]

            ;;;; ARITHMETIC
            [(eq? '* (car command))
                (*
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))]
            [(eq? '/ (car command))
                (quotient
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))]
            [(eq? '% (car command))
                (remainder
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))]
            [(eq? '+ (car command))
                (+
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))]
            [(eq? '- (car command))
                (if (null? (cddr command))
                    (* -1 
                        (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))
                    (-
                        (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                        (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)))]

            ;;;; COMPARISONS
            [(eq? '== (car command))
                (boolean-value (eq?
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)))]
            [(eq? '!= (car command))
                (boolean-value (not (eq?
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))))]
            [(eq? '< (car command))
                (boolean-value (<
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)))]
            [(eq? '> (car command))
                (boolean-value (>
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)))]
            [(eq? '<= (car command))
                (boolean-value (<=
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)))]
            [(eq? '>= (car command))
                (boolean-value (>=
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)))]

            ;;;; BOOLEAN OPERATORS
            [(eq? '&& (car command))
                (boolean-value (and
                    (eq? 'true (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))
                    (eq? 'true (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))))]
            [(eq? '|| (car command))
                (boolean-value (or
                    (eq? 'true (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))
                    (eq? 'true (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))))]
            [(eq? '! (car command))
                (boolean-value (not
                    (eq? 'true (evaluate-left-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))))]

            ;;;; COMMANDS THAT CAN PRODUCE VALUES
            [(eq? '= (car command))
                ;; Just get the value, ignore the assignment
                (evaluate-right-side
                        command state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw)]

            ;;;; FUNCTION CALL
            [(eq? 'funcall (car command))
                ((lambda (funcData)
                    (call/cc (lambda (k)
                        (car (execute-parse-tree
                            (cadr funcData)
                            ((caddr funcData)
                                (evaluate-list-of-values ; the values for the arg list
                                    (cddr command)
                                    state
                                    update-state-from-parse-tree
                                    update-state-from-command-list
                                    execute-parse-tree
                                    throw)
                                (update-state-from-command-list ; the state after evaluating the arg list
                                    (cddr command)
                                    state
                                    execute-parse-tree
                                    throw))
                            (lambda (v s)
                                (k v))
                            identity)))))
                    (get-var-value (cadr command) state))]

            ;;;; FALLBACK
            [else
                ;; Something went wrong...
                (error
                    "unknown command element"
                    (format "Did not recognize operation ~a" (car command)))])))

(define evaluate-list-of-values
    ; param command The parse trees to evaluate
    ; param state The state to use
    ; param update-state-from-parse-tree The function to call for updating state
    ; param update-state-from-command-list The function to call for updating state from a command list (like function arguments)
    ; param execute-parse-tree The function to call for executing a parse tree, such as a function body
    ; param throw The function to pass in the case of a function call for throw
    (lambda (commands state update-state-from-parse-tree update-state-from-command-list execute-parse-tree throw)
        (define evaluate-list-acc
            (lambda (remainingCommands state acc)
                (if (null? remainingCommands)
                    acc
                    (evaluate-list-acc
                        (cdr remainingCommands)
                        (update-state-from-parse-tree
                            (car remainingCommands)
                            state
                            execute-parse-tree
                            throw)
                        (append
                            acc
                            (list (evaluate-parse-tree
                                (car remainingCommands)
                                state
                                update-state-from-parse-tree
                                update-state-from-command-list
                                execute-parse-tree
                                throw)))))))
        (evaluate-list-acc commands state '())))

;;;; HELPER FUNCTIONS
(define evaluate-left-side
    (lambda (command state update-state-from-parse-tree update-state-from-command-list execute-parse-tree throw)
        (evaluate-parse-tree
            (cadr command) state
            update-state-from-parse-tree
            update-state-from-command-list
            execute-parse-tree
            throw)))

(define evaluate-right-side
    (lambda (command state update-state-from-parse-tree update-state-from-command-list execute-parse-tree throw)
        (evaluate-parse-tree
            (caddr command)
            (update-state-from-parse-tree
                (cadr command)
                state
                execute-parse-tree
                throw)
            update-state-from-parse-tree
            update-state-from-command-list
            execute-parse-tree
            throw)))

(define boolean-value
    (lambda (isTrue)
        (if isTrue
            'true
            'false)))

;; Gets the value of a single atom
(define get-atom-value
    ; param a The atom to evaluate
    ; param state The current state
    (lambda (a state)
        (if (or (number? a) (eq? a 'true) (eq? a 'false))
            a
            (get-var-value a state))))