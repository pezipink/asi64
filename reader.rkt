;Asi64
;Copyright Ross McKinlay, 2017
#lang racket
(require syntax/readerr)

(provide wrapper1
         make-asi-readtable)

(define (make-asi-readtable)
  (make-readtable (current-readtable)
                  #\{ 'terminating-macro read-lbrace
                  #\@ 'terminating-macro read-@
                  #\% 'terminating-macro read-%
                  #\£ 'terminating-macro read-£
                  #\$ 'terminating-macro read-$
                  ))

(define (wrapper1 thk)
  (parameterize ([current-readtable (make-asi-readtable)])
    (thk)))
 
(define read-lbrace
  (case-lambda
   [(ch in)
    (parse-6502-block in in (object-name in))]
   [(ch in src line col pos)
    (parse-6502-block in in src)]))

(define read-@
  (case-lambda
    [(ch in)
     #'#:immediate ]
    [(ch in src line col pos)
     #'#:immediate]))

(define read-£
  (case-lambda
    [(ch in)
     #'#:indirect ]
    [(ch in src line col pos)
     #'#:indirect]))

(define read-%
  (case-lambda
    [(ch in)
     (parse-number #\% (read in))]
    [(ch in src line col pos)
     (parse-number #\% (read in))]))

(define (read-hex-string in)
  (define (aux acc)
    (let ([c (peek-char in)])
      (cond
        [(string-contains? "0123456789abcdefABCDEF" (make-string 1 c))
         (aux (string-append acc (make-string 1 (read-char in))))]
        [else acc])))
  (aux ""))

(define read-$
  (case-lambda
    [(ch in)
     (parse-number #\$ (read-hex-string in))]
    [(ch in src line col pos)
      (parse-number #\$ (read-hex-string in))]))

(define (parse-number pre input)
  (let ([radix 
         (case pre
           [(#\$) 16]
           [(#\%) 2])]
         [str (cond [(number? input) (number->string input)]
                    [(symbol? input) (symbol->string input)]
                    [else input])])
    (string->number (string-replace str "_" "") radix)))

(define (parse-6502-block val in src)  
  (define (parse-6502-line acc paren-count)
    ; skip nested expressions, we assume a line ends at the first newline char
    ; outside of parens.
    (let ([c (peek-char in)])
      (cond
        [(and (equal? c #\;) (equal? paren-count 0))
         ;treat a comment as an end-of-line
         ;(writeln "comment")
         (begin
           (read-line in)
           
           `(6502-line ,@(reverse acc)))]
        
        [(and (equal? c #\newline) (equal? paren-count 0))
         ;(writeln "new line")
         (begin
           (read-char in)
           `(6502-line ,@(reverse acc)))]

        [(char-whitespace? c)
         (begin
           (read-char in)
           (parse-6502-line acc paren-count))]

        [(equal? c #\})
         `(6502-line ,@(reverse acc))]
        
        [else
         (parse-6502-line (cons [read-syntax "" in] acc) paren-count)])))

  (define (aux acc)
    (let ([c (peek-char in)])
      (cond
        [(equal? c #\})
         (begin
           (read-char in)
           `(6502-block ,@(reverse acc)))]
        [else
         (let ([c (parse-6502-line '() 0)])
           (if (equal? c '(6502-line))
               (aux acc)
               (aux (cons c acc))))])))

  (aux '()))
 
