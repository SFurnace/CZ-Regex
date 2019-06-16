#lang racket/base
(require "data-model.rkt" racket/port racket/format)
(provide parse-regex)

;;; Helper Functions

;; Any ... -> Nothing
(define (report-error . vs)
  (error (~a "parse-regex: " (apply ~a vs)
             "\nPosition: " (sub1 (file-position (current-input-port)))
             "\nRest: " (port->string (current-input-port)))))

;; [ Integer ] [ Port ] -> Void
(define (put-back-char [n 1] [port (current-input-port)])
  (file-position port (- (file-position port) n)))

;; String [ Port ] -> Boolean
(define (try-read-string str [port (current-input-port)])
  (let ([len (string-length str)])
    (cond
      [(equal? str (peek-string len 0 port)) (read-string len port) #t]
      [else #f])))

;; String [ Port ] -> Void
(define (expect-string str [port (current-input-port)])
  (let* ([len (string-length str)]
         [gotten (read-string len port)])
    (unless (equal? str gotten)
      (report-error "expect " str " but get " gotten))))

;;; Parser Core

;; String -> Regex
(define (parse-regex str)
  (parameterize ([current-input-port (open-input-string str)])
    (let ([result (read-choices)])
      (cond
        [(eof-object? (read-char)) result]
        [else (report-error "bad syntax")]))))

;; -> Choice
(define (read-choices)
  (let loop ([result '()])
    (define r (read-sequence))
    (if (try-read-string "|")
        (loop (cons r result))
        (Choice (reverse (cons r result))))))

;; -> Sequence
(define (read-sequence)
  (let loop ([result '()])
    (define c (read-char))
    (cond
      [(or (eof-object? c)) (Sequence (reverse result))]
      [(memv c (string->list "|)")) (put-back-char) (Sequence (reverse result))]
      [(eqv? c #\.) (loop (cons ANY result))]
      [(eqv? c #\() (loop (cons (read-group) result))]
      [(eqv? c #\[) (loop (cons (read-char-range) result))]
      [(eqv? c #\\) (loop (cons (read-backsplash-normal) result))]
      [(memv c (string->list "*+?{"))
       (when (or (null? result) (Repeat? (car result)))
         (report-error "nothing to repeat"))
       (loop (cons (read-repeat c (car result)) (cdr result)))]
      [else (loop (cons c result))])))

;; -> Group
(define (read-group)
  (define-values (lookahead? expected?)
    (cond
      [(try-read-string "?=") (values #t #t)]
      [(try-read-string "?!") (values #t #f)]
      [else (values #f #f)]))
  (let ([result (read-choices)])
    (expect-string ")")
    (Group result lookahead? expected?)))

;; -> (U Range Char)
(define (read-backsplash-normal)
  (let ([c (read-char)])
    (cond
      [(eof-object? c) (report-error "backsplash at end of the string")]
      [(eqv? c #\s) BLANK]
      [(eqv? c #\S) NON-BLANK]
      [(eqv? c #\d) NUMBER]
      [(eqv? c #\D) NON-NUMBER]
      [(eqv? c #\w) WORD]
      [(eqv? c #\W) NON-WORD]
      [(memv c (string->list "()[]{}.+*?\\")) c]
      [else (report-error "unkonwn escape sequence")])))

;; -> Range
(define (read-char-range)
  (define opposed? (try-read-string "^"))
  (let loop ([result '()])
    (define c (read-char))
    (cond
      [(eof-object? c) (report-error "end of stream when reading []")]
      [(eqv? c #\])
       (when (null? result) (report-error "nothing in char range"))
       (Range (reverse result) opposed?)]
      [(eqv? c #\-)
       (let ([next-c (peek-char)])
         (cond
           [(eof-object? next-c) (loop result)]
           [(char? (car result))
            (loop (cons (cons (car result) (read-char))
                        (cdr result)))]
           [else (report-error "bad char range pattern")]))]
      [(eqv? c #\\) (loop (cons (read-backsplash-in-char-range) result))]
      [else (loop (cons c result))])))

;; -> Char
(define (read-backsplash-in-char-range)
  (let ([c (read-char)])
    (cond
      [(eof-object? c)
       (report-error "end of stream when reading backsplash")]
      [(memv c (string->list "()[]{}.+*?|\\-^")) c]
      [else (report-error "unkonwn escape sequence")])))

;; Char Regex -> Repeat
(define (read-repeat type unit)
  (define-values (min max)
    (case type
      [(#\?) (values 0 1)]
      [(#\*) (values 0 +inf.0)]
      [(#\+) (values 1 +inf.0)]
      [(#\{) (read-repeat-times)]))
  (if (<= min max)
      (Repeat unit min max (not (try-read-string "?")))
      (report-error "bad order in {" min "," max "}")))

;; -> (Values Integer Integer)
(define (read-repeat-times)
  (define min (read-integer))
  (expect-string ",")
  (define max (read-integer))
  (expect-string "}")
  (values min max))

;; -> Ingeter
(define (read-integer)
  (define out (open-output-string))
  (let loop ([c (peek-char)])
    (cond
      [(eof-object? c) (report-error "end of stream in reading {,}")]
      [(char<=? #\0 c #\9)
       (write-char (read-char) out)
       (loop (peek-char))]
      [else (or (string->number (get-output-string out))
                (report-error "can't get integer"))])))
