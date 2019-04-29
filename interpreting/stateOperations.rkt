#lang racket
;;;; ***************************************************
;;;;   Group 40
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************

(provide
    create-state
    create-state-from-scope
    does-var-name-exist-in-state
    add-var-to-state
    set-var-value
    get-var-value
    push-empty-scope
    push-scope
    pop-scope
    get-current-scope
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
        (list (box '(() ())))))

;; Creates a new one-layer state with the given scope
(define create-state-from-scope list)

;; Checks if a varName exists in the given state
(define does-var-name-exist-in-state
    (lambda (varName state)
        (cond
            [(null? state)
                #f]
            [(does-var-exist-in-cur-scope? varName (get-current-scope-state state))
                #t]
            [else
                (does-var-name-exist-in-state varName (cdr state))])))

;; Creates a new state that has the new variable added to the front
(define add-var-to-state
    ; param varName is the name of the variable
    ; param state is the state to use
    (lambda (varName state)
        (if (does-var-exist-in-cur-scope? varName (get-current-scope-state state))
            (error
                "variable already initialized"
                (format "Variable ~a already exists in current scope" varName))
            (begin
                (set-box!
                    (car state)
                    (list
                        (cons varName (car (get-current-scope-state state)))
                        (cons '() (cadr (get-current-scope-state state)))))
                state))))

;; Creates a new state that has the variable with the correct value
(define set-var-value
    ; param varExpr is the name of the variable
    ; param varValue is the value to give the variable
    ; param state is the state to use
    (lambda (varExpr varValue state)
        ;;; (displayln 'SET-VAR-VALUE)
        ;;; (displayln varExpr)
        ;;; (displayln varValue)
        ;;; (displayln state)
        (cond
            [(null? state)
                (error 
                    "variable not initialized"
                    (format "No variable named ~a, cannot set value" varExpr))]
            [(and (pair? varExpr) (eq? 'dot (car varExpr)))
                (set-var-value
                    (caddr varExpr)
                    varValue
                    (get-var-value
                        (cadr varExpr)
                        state))]
            [(not (does-var-exist-in-cur-scope? varExpr (get-current-scope-state state)))
                (cons
                    (car state)
                    (set-var-value
                        varExpr
                        varValue
                        (pop-scope state)))]
            [else
                (begin
                    (set-box!
                        (car state)
                        (set-var-value-in-scope-state
                            varExpr
                            varValue
                            (get-current-scope-state state)))
                    state)])))

;; Gets a value given a variable name
(define get-var-value
    ; param varName The variable name
    ; param state The state to find the variable's value in
    (lambda (varName state)
        ;;; (displayln 'GET-VAR-VALUE)
        ;;; (displayln varName)
        ;;; (displayln (get-current-scope state))
        ;;; (displayln state)
        (cond
            [(null? state)
                (error 
                    "variable not initialized"
                    (format "No variable named ~a, cannot get value" varName))]
            [(not (does-var-exist-in-cur-scope? varName (get-current-scope-state state)))
                (get-var-value varName (pop-scope state))]
            [else
                (get-var-value-in-scope-state
                    varName
                    (get-current-scope-state state))])))

;; Adds a new layer for a new scope to a state
(define push-empty-scope
    ; param state The state to add a scope to
    (lambda (state)
        (cons (box '(() ())) state)))

;; Adds a new layer for a new scope to a state with given names and values
(define push-scope
    ; param varNames A list of variable names to add to the new scope
    ; param varValues A list of variable values to add to the new scope
    ; param state The state to add a scope to
    (lambda (varNames varValues state)
        (cons (box (list varNames varValues)) state)))

;; Removes the top layer of scope from a state
(define pop-scope cdr)

;; Gets the top layer of scope from a state
(define get-current-scope car)

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
        (unbox (car state))))

(define does-var-exist-in-cur-scope?
    (lambda (varName scopeState)
        (cond
            [(or (null? scopeState) (null? (car scopeState)))
                #f]
            [(eq? (caar scopeState) varName)
                #t]
            [else
                (does-var-exist-in-cur-scope?
                    varName
                    (go-to-next-var-in-scope scopeState))])))

(define go-to-next-var-in-scope
    (lambda (scopeState)
        (cons
            (cdar scopeState)
            (cons (cdadr scopeState) '()))))

(define go-to-next-var-in-state
    (lambda (state)
        (if (null? (cdr (get-current-scope-state state)))
            (pop-scope state)
            (cons
                (go-to-next-var-in-scope (get-current-scope-state state))
                (cdr state)))))

(define set-var-value-in-scope-state
    (lambda (varName varValue scopeState)
        (if (eq? (caar scopeState) varName)
            (list
                (car scopeState)
                (cons varValue (cdadr scopeState)))
            ((lambda (newScopeState)
                (list
                    (cons (caar scopeState) (car newScopeState))
                    (cons (caadr scopeState) (cadr newScopeState))))
                (set-var-value-in-scope-state
                    varName
                    varValue
                    (go-to-next-var-in-scope scopeState))))))

(define get-var-value-in-scope-state
    (lambda (varName scopeState)
        (if (eq? (caar scopeState) varName)
            (caadr scopeState)
            (get-var-value-in-scope-state varName (go-to-next-var-in-scope scopeState)))))