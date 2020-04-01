# CZ-Regex
A interpreter for regex search, just for learning.

## How to use
### `parse-regex` (-\> string? any/c)
parse a regex struct from string, for example:
```Racket
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
```

### `match-regex` (-\>\* (any/c string?) (natural-number/c) (or/c string? false/c))
match string with given regex, for example:
```Racket
(let ([r (parse-regex "http://([a-zA-Z0-9_]+\\.?)+(/.*?)(\\?[^ ]+)")]
      [str "http://www.bilibili.com/some/path?some=query   rest"])
  (match-regex r str))
;;=>
;;"http://www.bilibili.com/some/path?some=query"
```
