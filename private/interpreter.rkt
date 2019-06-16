#lang racket/base
(require "data-model.rkt" "parser.rkt" racket/match data/queue)
(provide parse-regex match-regex)

;;; Helper Functions

;; (U Char 'any) String Integer -> (U String Boolean)
(define (make-string-when-eqv char str index)
  (if (and (< index (string-length str))
           (or (eqv? char 'any)
               (eqv? char (string-ref str index))))
      (string (string-ref str index))
      #f))

(define (make-string-when-in-range char-min char-max str index)
  (if (and (< index (string-length str))
           (char<=? char-min (string-ref str index) char-max))
      (string (string-ref str index))
      #f))

;;; Interpreter Core
(struct Backtrack [new-max continuation])
(define backtrack-stack (make-parameter (void)))

;; -> (U String #f)
(define (try-backtrack)
  (if (non-empty-queue? (backtrack-stack))
      (let ([b (dequeue! (backtrack-stack))])
        ((Backtrack-continuation b) (Backtrack-new-max b)))
      #f))

;; Regex String [ Integer ] -> (U String #f)
(define (match-regex regex str [start 0])
  (parameterize ([backtrack-stack (make-queue)])
    (or (match-regex-core regex str start)
        (try-backtrack))))

;; Regex String Integer -> (U String #f)
(define (match-regex-core regex str start)
  (match regex
    [(Choice regexes)
     (match-regex-choices regexes str start)]
    [(Sequence regexes)
     (match-regex-seq regexes str start)]
    [(Group regex lookahead? expected?)
     (match-regex-group regex lookahead? expected? str start)]
    [(Range ranges opposed?)
     (match-regex-range ranges opposed? str start)]
    [(Repeat regex min max greedy?)
     (if greedy?
         (match-regex-repeat-greedy regex min max str start)
         (match-regex-repeat-non-greedy regex min max str start))]
    [(? char?)
     (make-string-when-eqv regex str start)]
    [_ (error "bad regex" regex)]))

;; (Listof Regex) String Integer -> (U String #f)
(define (match-regex-choices regexes str start)
  (for/or ([r (in-list regexes)]) (match-regex-core r str start)))

;; (Listof Regex) String Integer -> (U String #f)
(define (match-regex-seq regexes str start)
  (for/fold ([index start]
             [result ""] #:result result)
            ([r (in-list regexes)])
    (define r-result (match-regex-core r str index))
    #:final (not r-result)
    (if r-result
        (values (+ index (string-length r-result))
                (string-append result r-result))
        (values index #f))))

;; Regex Boolean Boolean String Integer -> (U String #f)
(define (match-regex-group regex lookahead? expected? str start)
  (let ([r (match-regex-core regex str start)])
    (cond
      [(and lookahead? expected? r) ""]
      [(and lookahead? (not expected?) r) #f]
      [else r])))

;; (Listof (U Char (Pairof Char Char))) Boolean String Integer -> (U String #f)
(define (match-regex-range ranges opposed? str start)
  (define result
    (for/or ([r (in-list ranges)])
      (cond
        [(char? r) (make-string-when-eqv r str start)]
        [(pair? r) (make-string-when-in-range (car r) (cdr r) str start)]
        [else (error "bad char range format" r)])))
  (cond
    [(and result (not opposed?)) result]
    [(and (not result) opposed?) (make-string-when-eqv 'any str start)]
    [else #f]))

;; Regex Integer Integer String Integer -> (U String #f)
(define (match-regex-repeat-greedy regex min max str start)
  (let loop ([i start] [times 0] [result ""])
    (cond
      [(= times max) result]
      [(match-regex-core regex str i)
       => (λ (s)
            (cond
              [(equal? s "") result]
              [else (loop (+ i (string-length s))
                          (add1 times)
                          (string-append result s))]))]
      [(<= min times max) result]
      [else #f])))

;; Regex Integer Integer String Integer -> (U String #f)
(define (match-regex-repeat-non-greedy regex min max str start)
  (let* ([continuation (void)]
         [new-max (call/cc (λ (c) (set! continuation c) min))])
    (cond
      [(and (<= new-max max)
            (<= new-max (add1 (- (string-length str) start))))  ;; 这里加一是为了空字符串匹配这种特殊情况
       (enqueue! (backtrack-stack)
                 (Backtrack (add1 new-max) continuation))
       (match-regex-repeat-greedy regex min new-max str start)]
      [else #f])))
