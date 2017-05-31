;Asi64                       `
;Copyright Ross McKinlay, 2017

#lang racket/base

(provide get-language-info)

(define (get-language-info data)
  (lambda (key default)
    (case key
      [(configure-runtime)
       '(#[asi64/lang/runtime-config configure #f])]
      [else default])))