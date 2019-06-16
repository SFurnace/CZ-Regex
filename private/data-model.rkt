#lang racket/base
(provide (all-defined-out))

;; Regex ::= (U Choice Sequence Group Range Repeat Char)

;; Choice ::= (Listof Regex)
(struct Choice (regexes) #:transparent)

;; Sequence ::= (Listof Regex)
(struct Sequence (regexes) #:transparent)

;; Regex ::= Regex Boolean Boolean
(struct Group (regex lookahead? expected?) #:transparent)

;; Range ::= (Listof (U Char (Pair Char Char))) Boolean
(struct Range (ranges opposed?) #:transparent)

;; Repeat ::= Regex Integer (U +inf.0 Integer) Boolean
(struct Repeat (regex min max greedy?) #:transparent)

(define ANY (Range '(#\newline) #t))
(define BLANK (Range '(#\space #\tab #\return #\vtab) #f))
(define NON-BLANK (Range '(#\space #\tab #\return #\vtab) #t))
(define NUMBER (Range '([#\0 . #\9]) #f))
(define NON-NUMBER (Range '([#\0 . #\9]) #t))
(define WORD (Range '([#\a . #\z] [#\A . #\Z] [#\0 . #\9] #\_) #f))
(define NON-WORD (Range '([#\a . #\z] [#\A . #\Z] [#\0 . #\9] #\_) #t))
