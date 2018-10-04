#lang racket
(require "lang/kara.rkt"
         "main.rkt")

(def !
  (let ([eq0   (compose eq (A id (K 0)))]
        [sub1  (compose - (A id (K 1)))])
    (cond [eq0  (K 1)]
          (compose * (A id (compose ! sub1))))))
(! 5)

(def dot-prod
  (compose (// + 0) (al *) trans))

(dot-prod '([1 2 3] [12 6 4]))

(def mat-mult
  ;; Argument of the form <m, n>
  (compose (al (al dot-prod))
     (al distl)
     distr
     (A (seq-ref 0  #|m|#)
        (compose trans (seq-ref 1)  #|trans n|#))))

(let ([test-pair '(([1 2 3] [4 5 6])
                   ([7 8] [9 10] [11 12]))])
  (displayln (mat-mult test-pair)))
