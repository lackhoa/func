#lang racket
(module old racket
        (provide (prefix-out old: cons)
                 (prefix-out old: length)
                 (prefix-out old: reverse)
                 (prefix-out old: eq?)
                 (prefix-out old: equal?)
                 (prefix-out old: +)
                 (prefix-out old: -)
                 (prefix-out old: *)
                 (prefix-out old: /)
                 (prefix-out old: not)
                 (prefix-out old: cond)
                 (prefix-out old: list-ref)
                 (prefix-out old: list?)
                 (prefix-out old: list)
                 (prefix-out old: car)
                 (prefix-out old: cdr)
                 (prefix-out old: map)
                 (prefix-out old: drop-right)))
(require 'old)
(provide (all-defined-out))

;;; Macros & short-hands
(define-syntax-rule (lam whatever ...)
  (lambda whatever ...))

(define-syntax-rule (def whatever ...)
  (define whatever ...))


(def None 'None  #|The Divine error code|#)

;;; Sequence-forming Forms, to protect list from containing None
(def (cons x seq)
  (match x
    ['None  None]
    [_      (if (list? seq)
                (old:cons x seq)
                None)]))

(define-syntax list
  ;; This is a macro, since we don't want eagerness,
  ;; and it's just a meta-function
  (syntax-rules ()
    [(_)            null]
    [(_ x1 x2 ...)  (match x1
                      ['None  None]
                      [_      (cons x1 (list x2 ...))])]))

;;; Functional Forms
(def ((pam . fs) x)
  ;; Construct (the opposite of `map`)
  (let loop ([res  null]
             [fs   fs])
    (match fs
      ['()          (old:reverse res)]
      [(cons f fs)  (match (f x)
                      ['None  None]
                      [fx     (loop (old:cons fx res)
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

(def ((k x) y)
  ;; Constant
  (match y
    ['None None]
    [_     x]))

(def ((// f default) xs)
  ;; Reduce/foldl (official name 'insert')
  (match xs
    ['()               default]
    [(cons xcar xcdr)  (f (list xcar
                                ((// f default) xcdr)))]
    [_                 None]))

(def ((map f) xs)
  ;; Apply to all
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (let loop ([res  null]
               [xs   xs])
      (match xs
        ['()               (old:reverse res)]
        [(cons xcar xcdr)  (match (f xcar)
                             ['None  None]
                             [fxcar  (loop (old:cons fxcar res)
                                           xcdr)])]))))

(def ((bu f x) y)
  ;; Binary to unary (currying)
  (f (cons x (cons y null)  #|Done to avoid invalid list|#)))

(def ((while p f) x)
  (match (p x)
    [#t  ((while p f) (f x))]
    [#f  x]
    [_   None]))

(def ((list-ref pos) seq)
  ;; List selector (kind of like python)
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (match pos
      [(or (? positive?) 0)  (old:list-ref seq pos)]
      [_                    (old:list-ref (old:reverse seq)
                                          (sub1 (- pos)))])))

(def (car xs)
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (old:car xs)))

(def (cdr seq)
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (old:cdr seq)))

(def id identity)

(def (show x)
  ;; The eye of God
  (begin (displayln x)
         (identity x)))

(def (comshow . fs)
  ;; The debugger of God
  (apply compose (add-between fs show)))

(def ((assert pred msg f ...) x)
  (if (pred x)
      x
      (error msg (f x) ...)))

;;; Booleans
(def (not x)
  (match x
    [#t  #f]
    [#f  #t]
    [_   None]))

(def or
  (let ([or?-binary  (lam (xy)
                       (match xy
                         [(list #t #t)  #t]
                         [(list #t #f)  #t]
                         [(list #f #t)  #t]
                         [(list #f #f)  #f]
                         [_             None]))])
    (// or?-binary #f)))

(def and
  (let ([and?-binary  (lam (xy)
                        (match xy
                          [(list #t #t)  #t]
                          [(list #t #f)  #f]
                          [(list #f #t)  #f]
                          [(list #f #f)  #f]
                          [_             None]))])
    (// and?-binary #t)))


;;; Rudimentary list functions
(def (list? x)
  (match x
    ['None  None]
    [_      (old:list? x)]))

(def (null? x)
  (match x
    ['None  None]
    ['()    #t]
    [_      #f]))

(def (length ls)
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (old:length ls)))

(def (reverse seq)
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (old:reverse seq)))

(def (distl x-ys)
  (let/ec escape
    (match x-ys
      [(list 'None _)         None]
      [(list x (? list? ys))  (for/list ([y  ys])
                                (match y
                                  ['None  (escape None)]
                                  [_      (list x y)]))]
      [_                      None])))

(def (distr xs-y)
  (let/ec escape
    (match xs-y
      [(list _ 'None)         None]
      [(list (? list? xs) y)  (for/list ([x  xs])
                                (match x
                                  ['None  (escape None)]
                                  [_      (list x y)]))]
      [_                      None])))

(def eq?
  (let ([eq?-binary  (lam (xy)
                       (match xy
                         [(list x y)  (old:eq? x y)]
                         [_           'None]))])
    (compose and
       (map eq?-binary)
       distl
       (pam car cdr))))

(def neq?
  (compose not eq?))

(def equal?
  (let ([equal?-binary  (lam (xy)
                          (match xy
                            [(list x y)  (old:equal? x y)]
                            [_           'None]))])
    (compose and
       (map equal?-binary)
       distl
       (pam car cdr))))

(def nequal?
  (compose not equal?))

;;; Arightmetic
(def (+ xy)
  (match xy
    [(list x y)  (with-handlers ([exn:fail?  (lambda (_)  'None)])
                   (old:+ x y))]
    [_           None]))

(def (- xy)
  (match xy
    [(list x y)  (with-handlers ([exn:fail?  (lambda (_)  'None)])
                   (old:- x y))]
    [_           None]))

(def (* xy)
  (match xy
    [(list x y)  (with-handlers ([exn:fail?  (lambda (_)  'None)])
                   (old:* x y))]
    [_           None]))

(def (/ xy)
  (match xy
    [(list x y)  (with-handlers ([exn:fail?  (lambda (_)  'None)])
                   (old:/ x y))]
    [_           None]))

(def (trans xss)
  ;; Transpose
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (match (eq? (old:map old:length xss))
      [#f  None]
      [#t  (apply old:map old:list xss)])))

(def (rcons xs-y)
  (match xs-y
    [(list (? list? xs) y)  (with-handlers ([exn:fail?  (lambda (_)  'None)])
                              (append xs `[,y]))]
    [_                      None]))

(def ((drop-right n) ls)
  (with-handlers ([exn:fail?  (lambda (_)  'None)])
    (old:drop-right ls n)))

;; Derived functions
(def rcdr
  (drop-right 1))

(def last
  (list-ref -1))

(def rotl
  (compose rcons (pam cdr
                (list-ref 0))))

(def rotr
  (compose cons (pam (list-ref -1)
               rcdr)))

(def (negate pred)
  (compose not pred))
