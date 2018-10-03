#lang racket
(require "lang/kara.rkt"
         "main.rkt")

(def !
  (let ([eq0   (compose eq (construct id
                                (const 0)))]
        [sub1  (compose - (construct id
                               (const 1)))])
    (condition
     [eq0  (const 1)]
     (compose * (construct id
                     (compose ! sub1))))))
(! 5)

(def dot-prod
  (compose (insert + 0) (alpha *) trans))

(dot-prod '([1 2 3] [12 6 4]))

(def mat-mult
  ;; Argument of the form <m, n>
  (compose (alpha (alpha dot-prod))
     (alpha distl)
     distr
     (construct (seq-ref 0  #|m|#)
                (compose trans (seq-ref 1)  #|trans n|#))))

(let ([test-pair '(([1 2 3] [4 5 6])
                   ([7 8] [9 10] [11 12]))]
      [step1     (construct (seq-ref 0  #|m|#)
                            (compose trans (seq-ref 1)  #|trans n|#))]
      [step2     distr]
      [step3     (alpha distl)]
      [step4     (alpha (alpha dot-prod))])
  (displayln (step1 test-pair))
  (displayln ((compose step2 step1) test-pair))
  (displayln ((compose step3 step2 step1) test-pair))
  (displayln ((compose step4 step3 step2 step1) test-pair)))
