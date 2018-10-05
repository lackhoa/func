#lang racket
(require "main.rkt"
         rackunit)

(test-case
 "Fib"
 (def !
   (let ([eq0?   (compose eq? (pam id (k 0)))]
         [sub1  (compose - (pam id (k 1)))])
     (cond [eq0?  (k 1)]
           (compose * (pam id (compose ! sub1))))))
 (check-eq? (! 5) 120))

(test-case
 "Dot product & matrix multiplication"
 (def dot-prod
   (compose (// + 0) (map *) trans))

 (check-eq? (dot-prod '([1 2 3] [12 6 4]))
            36)

 (def mat-mult
   ;; Argument of the form <m, n>
   (compose (map (map dot-prod))
      (map distl)
      distr
      (pam (list-ref 0  #|m|#)
           (compose trans (list-ref 1)  #|trans n|#))))
 (check-equal? (mat-mult '(([1 2 3] [4 5 6])
                           ([7 8] [9 10] [11 12])))
               '((58 64) (139 154)))
 (check-eq? (mat-mult '(([1 3] [4 5 6])
                        ([7 8] [9 10] [11 12])))
            None)
 (check-eq? (mat-mult '(1 2))
            None))

(test-case
 "Summation"
 (def sum (// + 0))
 (check-eq? (sum '(1 2 3 4 5))
            15)
 (check-eq? (sum '())
            0)
 (check-eq? (sum 4)
            None))

(test-case
 "Eq"
 (check-eq? (eq? '(1 1 1))
            #t)
 (check-eq? (eq? '(1 2))
            #f)
 (check-eq? (eq? None)
            None))

(test-case
 "Square"
 (check-eq? ((compose * (pam id id)) 5)
            25))
