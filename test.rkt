#lang racket/base
(require "main.rkt" rackunit)

(time
 (match-regex
  (parse-regex ".+?\\s?\\d{1,4}\\s*-\\d{1,2}-\\d{1,2}(\\w+)?.*?(png|jpg)")
  "cat 2019-1-1_1.png"))
