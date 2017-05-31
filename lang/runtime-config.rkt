;Asi64                       `
;Copyright Ross McKinlay, 2017

#lang racket/base

(provide configure)

(require (only-in asi64/reader make-asi-readtable))

(define (configure data)
  (current-readtable (make-asi-readtable)))