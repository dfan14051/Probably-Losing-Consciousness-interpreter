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
    evaluate-parse-tree)

;; Evaluates a list of commands
;; Returns the resulting value
(define evaluate-parse-tree
    ; param command The parse tree to execute
    ; param state The state to use
    ; param update-state-from-parse-tree The function to call for updating state
    (lambda (command state update-state-from-parse-tree)
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
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree))]
            [(eq? '/ (car command))
                (quotient
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree))]
            [(eq? '% (car command))
                (remainder
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree))]
            [(eq? '+ (car command))
                (+
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree))]
            [(eq? '- (car command))
                (if (null? (cddr command))
                    (* -1 
                        (evaluate-left-side
                        command state
                        update-state-from-parse-tree))
                    (-
                        (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                        (evaluate-right-side
                        command state
                        update-state-from-parse-tree)))]

            ;;;; COMPARISONS
            [(eq? '== (car command))
                (boolean-value (eq?
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree)))]
            [(eq? '!= (car command))
                (boolean-value (not (eq?
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree))))]
            [(eq? '< (car command))
                (boolean-value (<
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree)))]
            [(eq? '> (car command))
                (boolean-value (>
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree)))]
            [(eq? '<= (car command))
                (boolean-value (<=
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree)))]
            [(eq? '>= (car command))
                (boolean-value (>=
                    (evaluate-left-side
                        command state
                        update-state-from-parse-tree)
                    (evaluate-right-side
                        command state
                        update-state-from-parse-tree)))]

            ;;;; BOOLEAN OPERATORS
            [(eq? '&& (car command))
                (boolean-value (and
                    (eq? 'true (evaluate-left-side
                        command state
                        update-state-from-parse-tree))
                    (eq? 'true (evaluate-right-side
                        command state
                        update-state-from-parse-tree))))]
            [(eq? '|| (car command))
                (boolean-value (or
                    (eq? 'true (evaluate-left-side
                        command state
                        update-state-from-parse-tree))
                    (eq? 'true (evaluate-right-side
                        command state
                        update-state-from-parse-tree))))]
            [(eq? '! (car command))
                (boolean-value (not
                    (eq? 'true (evaluate-left-side
                        command state
                        update-state-from-parse-tree))))]

            ;;;; COMMANDS THAT CAN PRODUCE VALUES
            [(eq? '= (car command))
                ;; Just get the value, ignore the assignment
                (evaluate-right-side
                        command state
                        update-state-from-parse-tree)]

            ;;;; FALLBACK
            [else
                ;; Something went wrong...
                (error
                    "unknown command element"
                    (format "Did not recognize operation ~a" (car command)))])))

;;;; HELPER FUNCTIONS
(define evaluate-left-side
    (lambda (command state update-state-from-parse-tree)
        (evaluate-parse-tree
            (cadr command) state
            update-state-from-parse-tree)))

(define evaluate-right-side
    (lambda (command state update-state-from-parse-tree)
        (evaluate-parse-tree
            (caddr command)
            (update-state-from-parse-tree
                (cadr command) state)
            update-state-from-parse-tree)))

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