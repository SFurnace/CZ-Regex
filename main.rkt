#lang racket/base
(require "private/interpreter.rkt" racket/contract/base)
(provide (contract-out
          [parse-regex (-> string? any/c)]
          [match-regex (->* (any/c string?) (natural-number/c)
                            (or/c string? false/c))]))
