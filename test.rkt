#lang racket/base
(require "main.rkt" rackunit)


(parse-regex "(a|b|c)")
;;=>
;;(Choice
;; `(,(Sequence
;;     `(,(Group
;;         (Choice
;;          `(,(Sequence `(#\a))
;;            ,(Sequence `(#\b))
;;            ,(Sequence `(#\c))))
;;         false
;;         false)))))

(parse-regex "[^a-z_]{1,2}")
;;=>
;;(Choice
;; `(,(Sequence
;;     `(,(Repeat (Range `((#\a . #\z) #\_) true) 1 2 true)))))

(let ([r (parse-regex "http://([a-zA-Z0-9_]+\\.?)+(/.*?)(\\?[^ ]+)")]
      [str "http://www.bilibili.com/some/path?some=query   rest"])
  (match-regex r str))
;;=>
;;"http://www.bilibili.com/some/path?some=query"
