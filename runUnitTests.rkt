#lang racket

(require
    "interpreting/interpreter.rkt")

(define run-unit-test
    (lambda (unitTests)
        (with-handlers
            ([exn?
                (lambda (exn)
                    (if (cadar unitTests)
                        (writeln (format "~a failed; unexpected exception" (caar unitTests)))
                        (writeln (format "~a succeeded" (caar unitTests)))))])
            (let
                ([result
                    (interpret (format "unit_tests/unit_test_~a.txt" (caar unitTests)))])
                (cond
                    [(eq? (caddar unitTests) result)
                        (writeln (format "~a succeeded" (caar unitTests)))]
                    [(cadar unitTests)
                        (writeln (format "~a failed; expected ~v, got ~v" (caar unitTests) (caddar unitTests) result))]
                    [else
                        (writeln (format "~a did not throw when expected to" (caar unitTests)))])))
        (if (null? (cdr unitTests))
            "Done"
            (run-unit-test (cdr unitTests)))))

(run-unit-test '(
        ("01" #t 150)
        ("02" #t -4)
        ("03" #t 10)
        ("04" #t 16)
        ("05" #t 220)
        ("06" #t 5)
        ("07" #t 6)
        ("08" #t 10)
        ("09" #t 5)
        ("10" #t -39)
        ("11" #f ())
        ("12" #f ())
        ("13" #f ())
        ("14" #f ())
        ("15" #t true)
        ("16" #t 100)
        ("17" #t false)
        ("18" #t true)
        ("19" #t 128)
        ("20" #t 12)
        ("21" #t 30)
        ("22" #t 11)
        ("23" #t 1106)
        ("24" #t 12)
        ("25" #t 16)
        ("26" #t 72)
        ("27" #t 21)
        ("28" #t 164)
))