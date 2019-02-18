#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(provide
    create-state
    add-var-to-state
    set-var-value
    get-var-value
    push-scope
    pop-scope
    return-value
    state-value)

;; Creates a new empty state
(define create-state
    ;; Scope has the following format:
    ;;  '(
    ;;      ((deepScopeVar) (deepScopeValue))
    ;;      ((shallowScopeVar1 shallowScopeVar2) (shallowScopeValue1 shallowScopeValue2))
    ;;   )
    ;; This allows for easily pushing and popping scopes as necessary
    (lambda ()
        '((() ()))))

;; Creates a new state that has the new variable added to the front
(define add-var-to-state
    ; param varName is the name of the variable
    ; param state is the state to use
    (lambda (varName state)
        (cons 
            (cons (cons varName (caar state)) (cons (cons '() (cadar state)) '()))
            (cdr state))))

;; Creates a new state that has the variable with the correct value
(define set-var-value
    ; param varName is the name of the variable
    ; param varValue is the value to give the variable
    ; param state is the state to use
    (lambda (varName varValue state)
        (cond
            [(null? state)
                (error 
                    "variable not initialized"
                    (format "No variable named ~a, cannot set value" varName))]
            [(eq? (caaar state) varName)
                (cons
                    (cons (caar state) (cons (cons varValue (cdadar state)) '()))
                    (cdr state))]
            [else
                (let
                    ([newState
                        (set-var-value varName varValue (go-to-next-var-in-state state))])
                    (if (null? (cdar state))
                        (cons (car state) newState)
                        (cons
                            (cons
                                (cons (caaar state) (caar newState))
                                (cons (cons (caadar state) (cadar newState)) '()))
                            (cdr newState))))])))

;; Gets a value given a variable name
(define get-var-value
    ; param varName The variable name
    ; param state The state to find the variable's value in
    (lambda (varName state)
        (cond
            [(null? state)
                (error 
                    "variable not initialized"
                    (format "No variable named ~a, cannot get value" varName))]
            [(null? (caar state))
                (get-var-value varName (pop-scope state))]
            [(eq? (caaar state) varName)
                (caadar state)]
            [else
                (get-var-value varName (go-to-next-var-in-state state))])))

;; Adds a new layer for a new scope to a state
(define push-scope
    ; param state The state to add a scope to
    (lambda (state)
        (cons '(() ()) state)))

;; Removes the top layer of scope from a state
(define pop-scope
    ; param state The state to remove a scope from
    (lambda (state)
        (cdr state)))

;; Returns the value of the return statement
(define return-value
    ; param value The value to return
    (lambda (value)
        (if (pair? value)
            (car value)
            value)))

;; Returns the value of the state
(define state-value
    ; param value The value to return
    (lambda (value)
        (if (and (pair? value) (not (pair? (car value))))
            (cadr value)
            value)))

;;;; ***************************************************
;;;;   HELPER FUNCTIONS
;;;; ***************************************************
(define get-current-scope-state
    (lambda (state)
        (car state)))

(define go-to-next-var-in-scope
    (lambda (scopeState)
        (cons
            (cdar scopeState)
            (cons (cdadr scopeState) '()))))

(define go-to-next-var-in-state
    (lambda (state)
        (if (null? (cdar state))
            (pop-scope state)
            (cons
                (go-to-next-var-in-scope (get-current-scope-state state))
                (cdr state)))))