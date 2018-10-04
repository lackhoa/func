#lang racket
(module old racket
        (provide (prefix-out old: cons)
                 (prefix-out old: length)
                 (prefix-out old: reverse)
                 (prefix-out old: +)
                 (prefix-out old: -)
                 (prefix-out old: *)
                 (prefix-out old: /)
                 (prefix-out old: not)
                 (prefix-out old: cond)))
(require "lang/kara.rkt"
         'old)
(provide (all-defined-out))

(def None 'None  #|The Divine error code|#)

;;; Sequence-forming Forms
;;; These are all macross since we don't want eager evaluation
(define-syntax-rule (cons x seq)
  (match x
    ['None  None]
    [_      (match seq
              ['None  None]
              [_      (old:cons x seq)])]))

(define-syntax list
  (syntax-rules ()
    [(_)            null]
    [(_ x1 x2 ...)  (cons x1 (list x2 ...))]))

;;; Functional Forms
(def ((a . fs) x)
  ;; Construct
  (let loop ([res  null]
             [fs   fs])
    (match fs
      ['()          (old:reverse res)]
      [(cons f fs)  (match (f x)
                      ['None  'None]
                      [fx     (loop (cons fx res)
                                    fs)])])))

(define-syntax-rule (cond [p f]
                          ...
                          g)
  ;; Like condition, but if the predicate doesn#t
  ;; return a boolean then fuck it
  (lam (x)
    (let/ec escape
      (old:cond [(match (p x)
                   [#t  #t]
                   [#f  #f]
                   [_   (escape None)])  (f x)]
                ...
                [else                    (g x)]))))

(def (fuck-none f)
  (lam (x)
    (match x
      ['None  'None]
      [_      (f x)])))

(def (k x)
  ;; Constant
  (fuck-none (lam (_) x)))

(def ((// f default) xs)
  ;; Reduce/foldl (official name being 'insert')
  (match xs
    ['()               default]
    [(cons xcar xcdr)  (f (list xcar
                                ((// f default) xcdr)))]
    [_                 None]))

(def ((al f) xs)
  ;; Apply to all
  (if (list? xs) (map f xs) None))

(def ((bu f x) y)
  ;; Binary to unary (currying)
  (f (cons x (cons y null)  #|Done like this to avoid invalid list|#)))

(def ((while p f) x)
  (match (p x)
    [#t  ((while p f) (f x))]
    [#f  x]
    [_   None]))

(def ((seq-ref pos) seq)
  ;; Sequence selector
  (and (< pos (length seq))
     (list-ref seq pos)))

(def (tl seq)
  ;; cdr
  (and (list? seq) (cdr seq)))

(def id identity)

(def (show x)
  ;; The eye of God
  (begin (displayln x)
         (identity x)))

(def (comshow . fs)
  ;; The debugger of God
  (apply compose (add-between fs show)))

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

(def (trans xss)
  (and (list? xss)
     (forall? list? xss)
     (apply eq*? (map length xss))
     (apply map list xss)))

(def (& xy)
  (match xy
    [(list #t #t)  #t]
    [(list #t #f)  #f]
    [(list #f #t)  #f]
    [(list #f #f)  #f]
    [_             #f]))

(def (v xy)
  (match xy
    [(list #t #t)  #t]
    [(list #t #f)  #t]
    [(list #f #t)  #t]
    [(list #f #f)  #f]
    [_             #f]))

(def (not x)
  (match x
    [#t  #f]
    [#f  #t]
    [_   #f]))

(def (apndl x-ys)
  (match x-ys
    [(list x (? list? ys))  (cons x ys)]
    [_                      #f]))

(def (apndr xs-y)
  (match xs-y
    [(list (? list? xs) y)  (pad xs y)]
    [_                      #f]))

(def (tlr xs)
  (and (list? xs)
     (drop-right xs 1)))

;; Derived functions
(def (seq-refr pos)
  (compose (seq-ref pos) reverse))

(def rotl
  (compose apndr (a tl
              (seq-ref 0))))

(def rotr
  (compose apndl (a (seq-refr 0)
              tlr)))

(def last
  (cond [(compose null tl)  (seq-ref 0)]
        (compose last tl)))
