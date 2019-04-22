#lang racket
;;;; ***************************************************
;;;;   Group 40
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
                            return throw break)))
                    return throw break)]
            [(eq? 'funcall (caar parseTree))
                ((lambda (result)
                    (execute-parse-tree
                        (cdr parseTree)
                        state
                        return throw break))
                    (evaluate-parse-tree
                        (car parseTree)
                        state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))]

            ;; Return, throw, break, continue
            [(eq? 'return (caar parseTree))
                (return 
                    (evaluate-parse-tree 
                        (cadar parseTree)
                        state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))]
            [(eq? 'throw (caar parseTree))
                (throw
                    (evaluate-parse-tree 
                        (cadar parseTree)
                        state
                        update-state-from-parse-tree
                        update-state-from-command-list
                        execute-parse-tree
                        throw))]
            [(eq? 'break (caar parseTree))
                (if (null? break)
                    (error
                        "illegal break"
                        "Cannot break when not in a loop")
                    (break))]
            [(eq? 'continue (caar parseTree))
                (if (null? break)
                    (error
                        "illegal continue"
                        "Cannot continue when not in a loop")
                    (list '()))]

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
                        state
                        execute-parse-tree
                        throw)
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
                (cadar parseTree)
                state
                update-state-from-parse-tree
                update-state-from-command-list
                execute-parse-tree
                throw)
            (update-state-from-parse-tree
                (cadar parseTree)
                state
                execute-parse-tree
                throw))))

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
                    (cadar parseTree)
                    state
                    update-state-from-parse-tree
                    update-state-from-command-list
                    execute-parse-tree
                    throw)
            (update-state-from-parse-tree
                    (cadar parseTree)
                    state
                    execute-parse-tree
                    throw))))

(define execute-try-return-new-state
    (lambda (parseTree baseState baseReturn baseThrow baseBreak)
        ((lambda (doFinally)
            ((lambda (doCatch)
                 (call/cc (lambda (k) (doFinally (cadr
                    (execute-parse-tree
                        (cadar parseTree)
                        (push-scope baseState)
                        (lambda (v)
                            (k (baseReturn
                                v
                                (doFinally))))
                        (lambda (e)
                            (k
                                (doFinally (cadr
                                    (doCatch e)))))
                        (if (null? baseBreak)
                            '()
                            (lambda ()
                                (k (baseBreak
                                    (doFinally)))))))))))
                (lambda (exception)
                    (if (null? (caddar parseTree))
                        '()
                        (call/cc (lambda (k) (execute-parse-tree
                            (caddr (caddar parseTree))
                            (set-var-value
                                (caadr (caddar parseTree))
                                exception
                                (add-var-to-state
                                    (caadr (caddar parseTree))
                                    (push-scope baseState)))
                            (lambda (v)
                                (k (baseReturn
                                    v)))
                            (lambda (e)
                                (k (baseThrow
                                    e)))
                            (if (null? baseBreak)
                                '()
                                (lambda (s)
                                    (k (baseBreak)))))))))))
            (lambda (preFinallyState)
                (if (null? (car (cdddar parseTree)))
                    preFinallyState
                    (cadr (execute-parse-tree
                        (cadar (cdddar parseTree))
                        preFinallyState
                        baseReturn baseThrow baseBreak)))))))