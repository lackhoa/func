#lang racket
(module old racket
        (provide (prefix-out old: cons)
                 (prefix-out old: length)
                 (prefix-out old: reverse)
                 (prefix-out old: +)
                 (prefix-out old: -)
                 (prefix-out old: *)
                 (prefix-out old: /)
                 (prefix-out old: not)))
(require "lang/kara.rkt"
         'old)

;;; Functional Forms
(def ((construct . fs) x)
  (let/ec escape
    (and (list? fs)
       x
       (for/list ([f  fs])
         (match (f x)
           [#f  (escape #f)]
           [fx  fx])))))

(define-syntax condition
  (syntax-rules ()
    [(_ [p1 f1] [p2 f2] ... g)
     (lam (x)
       (let/ec escape
         (cond [(match (p1 x)
                  ['T  #t]
                  ['F  #f]
                  [_   (escape #f)])  (f1 x)]
               [(match (p2 x)
                  ['T  #t]
                  ['F  #f]
                  [_   (escape #f)])  (f2 x)]
               ...
               [else  (g x)])))]))

(def ((const x) y)
  (and y x))

(def ((insert )))

;; #f stands for the bottom value, 'T and 'F stand for true and false
(def (bsym b)  (match b [#t  'T] ['F  #f]))
(def (rbsym s) (match s ['T  #t] ['F  #f]))

(def (cons x seq)
  ;; Sequence-forming
  (and x (old:cons x seq)))

(def ((seq-ref pos) seq)
  ;; Sequence selector
  (and (< pos (length seq))
     (list-ref seq pos)))

(def (tl seq)
  ;; cdr
  (and (list? seq) (cdr seq)))

(def id identity)

;;; Booleans
(def (atom x)
  (and x (compose bsym old:not list?)))

(def (eq pair)
  (and pair
     (match pair
       [(list x y)  (bsym (eq? x y))]
       [_           #f])))

(def (null seq)
  (and seq
     (bsym (null? seq))))

(def (reverse seq)
  (and (list? seq)
     (old:reverse seq)))

(def (distl x-ys)
  (match x-ys
    [(list x (? list? ys))  (for/list ([y  ys])
                              (list x y))]
    [_                      #f]))

(def (distr xs-y)
  (match xs-y
    [(list (? list? xs) y)  (for/list ([x  xs])
                              (list x y))]
    [_                      #f]))

(def (length ls)
  (and (list? ls)
     (old:length ls)))

;;; Arightmetic
(def (+ xy)
  (match xy
    [(list x y)  (and (number? x)
                    (number? y)
                    (old:+ x y))]
    [_           #f]))

(def (- xy)
  (match xy
    [(list x y)  (and (number? x)
                    (number? y)
                    (old:- x y))]
    [_           #f]))

(def (* xy)
  (match xy
    [(list x y)  (and (number? x)
                    (number? y)
                    (old:* x y))]
    [_           #f]))

(def (/ xy)
  (match xy
    [(list x y)  (and (number? x)
                    (number? y)
                    (old:not (eq? y 0))
                    (old:/ x y))]
    [_           #f]))

(def (transpose xss)
  (and (list? xss)
     (forall? list? xss)
     (apply eq*? (map length xss))
     (apply map list xss)))

(def (& xy)
  (match xy
    [(list 'T 'T)  'T]
    [(list 'T 'F)  'F]
    [(list 'F 'T)  'F]
    [(list 'F 'F)  'F]
    [_             #f]))

(def (v xy)
  (match xy
    [(list 'T 'T)  'T]
    [(list 'T 'F)  'T]
    [(list 'F 'T)  'T]
    [(list 'F 'F)  'F]
    [_             #f]))

(def (not x)
  (match x
    ['T  'F]
    ['F  'T]
    [_   #f]))

(def (apndl x-ys)
  (match x-ys
    [(list x (? list? ys))  (cons x ys)]
    [_                      #f]))

(def (apndr xs-y)
  (match xs-y
    [(list (? list? xs) y)  (pad xs y)]
    [_                      #f]))

(def (seq-refr pos)
  (compose (seq-ref pos) reverse))

(def (tlr xs)
  (and (list? xs)
     (drop-right xs 1)))

(def (rotl xs)
  (compose apndr (construct tl (seq-ref 1))))

(def (rotr xs)
  (compose apndl (construct (seq-refr 1) tlr)))
