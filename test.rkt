#lang racket
(require "lang/kara.rkt")
(provide (all-defined-out))

(module old racket
        (require "lang/kara.rkt")
        (provide plus)

        (def plus +))

(require 'old)
(def + plus)
