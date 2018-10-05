#lang racket
(require "lang/kara.rkt"
         "main.rkt")

(def !
  (let ([eq0?   (compose eq? (pam id (k 0)))]
        [sub1  (compose - (pam id (k 1)))])
    (cond [eq0?  (k 1)]
          (compose * (pam id (compose ! sub1))))))
(! 5)

(def dot-prod
  (compose (// + 0) (map *) trans))

(dot-prod '([1 2 3] [12 6 4]))

(def mat-mult
  ;; Argument of the form <m, n>
  (compose (map (map dot-prod))
     (map distl)
     distr
     (pam (list-ref 0  #|m|#)
          (compose trans (list-ref 1)  #|trans n|#))))

(let ([test-pair '(([1 2 3] [4 5 6])
                   ([7 8] [9 10] [11 12]))])
  (displayln (mat-mult test-pair)))
