;Asi64                       `
;Copyright Ross McKinlay, 2017

#lang s-exp syntax/module-reader
asi64
#:wrapper1 wrapper1
#:language-info #(asi64/lang/language-info get-language-info #f)

(require "../reader.rkt")