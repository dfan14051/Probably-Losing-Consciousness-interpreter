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
    get-var-value)

;; Creates a new empty state
(define create-state
    (lambda ()
        '(() ())))

;; Creates a new state that has the new variable added to the front
(define add-var-to-state
    ; param varName is the name of the variable
    ; param state is the state to use
    (lambda (varName state)
        (cons (cons varName (car state)) (cons (cons '() (cadr state)) '()))))

;; Creates a new state that has the variable with the correct value
(define set-var-value
    ; param varName is the name of the variable
    ; param varValue is the value to give the variable
    ; param state is the state to use
    (lambda (varName varValue state)
        (cond
            [(null? (car state))
                (error 
                    "variable not initialized"
                    (format "No variable named ~a" varName))]
            [(eq? (caar state) varName)
                (cons (cons varName (cdar state)) (cons (cons varValue (cdadr state)) '()))]
            [else
                (cons
                    (car state)
                    (set-var-value
                        varName
                        varValue 
                        (go-to-next-var-in-state state)))])))

;; Gets a value given a variable name
(define get-var-value
    (lambda (varName state)
        (cond
            [(null? (car state))
                (error 
                    "variable not initialized"
                    (format "No variable named ~a" varName))]
            [(eq? (caar state) varName)
                (caadr state)]
            [else
                (get-var-value varName (go-to-next-var-in-state state))])))

;;;; ***************************************************
;;;;   HELPER FUNCTIONS
;;;; ***************************************************
(define go-to-next-var-in-state
    (lambda (state)
        (cons (cdar state) (cons (cdadr state) '()))))