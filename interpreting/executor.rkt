#lang racket
;;;; ***************************************************
;;;;   Group 36 (formerly group 40)
;;;;   Noah Crowley (nwc17) and David Fan (dmf98) and Lucia Wei (llw44)
;;;;   EECS 345 Spring 2019
;;;;   Interpreter Project
;;;; ***************************************************
(require
    "stateOperations.rkt"
    "evaluator.rkt"
    "stateUpdater.rkt")

(provide
    execute-parse-tree)

(define execute-parse-tree
    (lambda (parseTree state return throw [break '()])
        (cond
            ;; Base cases
            [(null? parseTree)
                (list '() state)]

            ;; Code blocks
            [(eq? 'begin (caar parseTree))
                (execute-parse-tree
                    (cdr parseTree)
                    (pop-scope (cadr
                        (execute-parse-tree
                            (cdar parseTree)
                            (push-scope state)
                            (lambda (v s)
                                (return v (pop-scope s)))
                            (lambda (e s)
                                (throw e (pop-scope s)))
                            (lambda (s)
                                (break (pop-scope s))))))
                    return throw break)]

            ;; Return, throw, break, continue
            [(eq? 'return (caar parseTree))
                (return 
                    (evaluate-parse-tree 
                        (cadar parseTree) state
                        update-state-from-parse-tree) 
                    (update-state-from-parse-tree 
                        (cadar parseTree)
                        state))]
            [(eq? 'throw (caar parseTree))
                (throw
                    (evaluate-parse-tree 
                        (cadar parseTree) state
                        update-state-from-parse-tree) 
                    (update-state-from-parse-tree 
                        (cadar parseTree)
                        state))]
            [(eq? 'break (caar parseTree))
                (if (null? break)
                    (error
                        "illegal break"
                        "Cannot break when not in a loop")
                    (break state))]
            [(eq? 'continue (caar parseTree))
                (if (null? break)
                    (error
                        "illegal continue"
                        "Cannot continue when not in a loop")
                    (list '() state))]

            ;; Try-catch-finally
            [(eq? 'try (caar parseTree))
                (execute-parse-tree
                    (cdr parseTree)
                    (execute-try-return-new-state
                        parseTree
                        state
                        return throw break)
                    return throw break)]

            ;; If and while
            [(eq? 'if (caar parseTree))
                (execute-parse-tree
                    (cdr parseTree)
                    (execute-if-return-new-state
                        parseTree state
                        return throw break)
                    return throw break)]
            [(eq? 'while (caar parseTree))
                (execute-parse-tree
                    (cdr parseTree)
                    (call/cc
                        (lambda (while-break)
                            (execute-while-return-new-state
                                parseTree state
                                break
                                return throw while-break)))
                    return throw break)]
            [else
                (execute-parse-tree
                    (cdr parseTree)
                    (update-state-from-parse-tree
                        (car parseTree)
                        state)
                    return throw break)])))

;;;; HELPER FUNCTIONS

(define execute-if-return-new-state
    (lambda (parseTree state return throw break)
        ((lambda (conditionResult postConditionState)
            (cond
                [(eq? conditionResult 'true)
                    (cadr (execute-parse-tree
                        (list (caddar parseTree))
                        postConditionState
                        return throw break))]
                [(null? (cdddar parseTree))
                    postConditionState]
                [else
                    (cadr (execute-parse-tree
                        (list (car (cdddar parseTree)))
                        postConditionState
                        return throw break))]))
            (evaluate-parse-tree
                (cadar parseTree) state
                update-state-from-parse-tree)
            (update-state-from-parse-tree (cadar parseTree) state))))

(define execute-while-return-new-state
    (lambda (parseTree state old-break return throw break)
        ((lambda (conditionResult postConditionState)
            (if (eq? conditionResult 'true)
                (execute-while-return-new-state
                    parseTree
                    (cadr (execute-parse-tree
                        (list (caddar parseTree))
                        postConditionState
                        return throw break))
                    old-break
                    return throw break)
                postConditionState))
            (evaluate-parse-tree
                    (cadar parseTree) state
                    update-state-from-parse-tree)
            (update-state-from-parse-tree
                    (cadar parseTree) state))))

(define execute-try-return-new-state
    (lambda (parseTree baseState baseReturn baseThrow baseBreak)
        ((lambda (doFinally)
            ((lambda (doCatch)
                 (call/cc (lambda (k) (doFinally (cadr
                    (execute-parse-tree
                        (cadar parseTree)
                        (push-scope baseState)
                        (lambda (v s)
                            (k (baseReturn
                                v
                                (doFinally (pop-scope s)))))
                        (lambda (e s)
                            (k
                                (doFinally (cadr
                                    (doCatch e (pop-scope s))))))
                        (if (null? baseBreak)
                            '()
                            (lambda (s)
                                (k (baseBreak
                                    (doFinally (pop-scope s))))))))))))
                (lambda (exception preCatchState)
                    (if (null? (caddar parseTree))
                        (list '() preCatchState)
                        (call/cc (lambda (k) (execute-parse-tree
                            (caddr (caddar parseTree))
                            (set-var-value
                                (caadr (caddar parseTree))
                                exception
                                (add-var-to-state
                                    (caadr (caddar parseTree))
                                    (push-scope preCatchState)))
                            (lambda (v s)
                                (k (baseReturn
                                    v
                                    (doFinally (pop-scope s)))))
                            (lambda (e s)
                                (k (baseThrow
                                    e
                                    (doFinally (pop-scope s)))))
                            (if (null? baseBreak)
                                '()
                                (lambda (s)
                                    (k (baseBreak
                                        (doFinally (pop-scope s)))))))))))))
            (lambda (preFinallyState)
                (if (null? (car (cdddar parseTree)))
                    preFinallyState
                    (cadr (execute-parse-tree
                        (cadar (cdddar parseTree))
                        preFinallyState
                        baseReturn baseThrow baseBreak)))))))