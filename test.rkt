#lang racket
(require "main.rkt" rackunit racket/pretty)

(time
 (match-regex
  (parse-regex "\\?*?\\+")
  "???+"))
