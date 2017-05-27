#lang racket

(require (for-syntax syntax/parse))
(require (for-syntax racket/string))
(require (for-syntax racket/base))
(require (for-syntax racket/match))
(require (for-syntax racket/syntax))
(require (for-syntax racket/list))
(define is-debug #f)

(define-syntax (wdb stx)  
  (syntax-parse stx
    [(_ text)
     #'(when is-debug
         (writeln text))]
  [(_ text args ...)
   #'(when is-debug
       (writeln (format text args ...)))]))

(define (lo-byte input)
  (bitwise-and input #xFF))

(define (hi-byte input)
  (arithmetic-shift
   (bitwise-and input #xFF00)
   -8))

(define (partial-address-or-8bit value)
  (if (symbol? value)
      value
      (lo-byte value)))

(define-match-expander 8bit
  (λ (stx)
    (syntax-case stx ()
      [(_ v)
       #'(app partial-address-or-8bit (? identity v))
       ])))

(define (extract-little-endian-address input)
  (if (symbol? input)
      input
      (cons (lo-byte input) (hi-byte input))))

(define-match-expander 16bit
  (λ (stx)
    (syntax-case stx ()
      [(_ v )
       #'(app extract-little-endian-address (? identity v))])))

(struct transition (type label))
(define (to-bytes input)
  (wdb "matching ~a" input)
  (match input
    [(list 'break 'i #f)        (set-add! (context-breakpoints prog) (context-location prog))]
;    [(list '+= 'i #f)       (set-location t)]
     ; ORA
    [(list 'ora 'zpxi (8bit x) )(list #x01 x)]
    [(list 'ora 'zp   (8bit x) )(list #x05 x)]
    [(list 'ora 'i    (8bit x) )(list #x09 x)]
    [(list 'ora 'abs  (16bit x) )(list #x0D x)]
    [(list 'ora 'zpyi (8bit x) )(list #x11 x)]
    [(list 'ora 'zpx  (8bit x) )(list #x15 x)]
    [(list 'ora 'absy (16bit x) )(list #x19 x)]
    [(list 'ora 'absx (16bit x) )(list #x1D x)]

    ; AND
    [(list 'and 'zpxi (8bit x) )(list #x21 x)]
    [(list 'and 'zp   (8bit x) )(list #x25 x)]
    [(list 'and 'i    (8bit x) )(list #x29 x)]
    [(list 'and 'abs  (16bit x) )(list #x2D x)]
    [(list 'and 'zpyi (8bit x) )(list #x31 x)]
    [(list 'and 'zpx  (8bit x) )(list #x35 x)]
    [(list 'and 'absy (16bit x) )(list #x39 x)]
    [(list 'and 'absx (16bit x) )(list #x3D x)]

    ;EOR
    [(list 'eor 'zpxi (8bit x) )(list #x41 x)]
    [(list 'eor 'zp   (8bit x) )(list #x45 x)]
    [(list 'eor 'i    (8bit x) )(list #x49 x)]
    [(list 'eor 'abs  (16bit x) )(list #x4D x)]
    [(list 'eor 'zpyi (8bit x) )(list #x51 x)]
    [(list 'eor 'zpx  (8bit x) )(list #x55 x)]
    [(list 'eor 'absy (16bit x) )(list #x59 x)]
    [(list 'eor 'absx (16bit x) )(list #x5D x)]

    ;ADC
    [(list 'adc 'zpxi (8bit x) )(list #x61 x)]
    [(list 'adc 'zp   (8bit x) )(list #x65 x)]
    [(list 'adc 'i    (8bit x) )(list #x69 x)]
    [(list 'adc 'abs  (16bit x) )(list #x6D x)]

    
    [(list 'adc 'zpyi (8bit x) )(list #x71 x)]
    [(list 'adc 'zpx  (8bit x) )(list #x75 x)]
    [(list 'adc 'absy (16bit x) )(list #x79 x)]
    [(list 'adc 'absx (16bit x) )(list #x7D x)]

    ;STA
    [(list 'sta 'zpxi (8bit x) )(list #x81 x)]
    [(list 'sta 'zp   (8bit x) )(list #x85 x)]
    ; no immiediate 
    [(list 'sta 'abs  (16bit x) )(list #x8D x)]
    [(list 'sta 'zpyi (8bit x) )(list #x91 x)]
    [(list 'sta 'zpx  (8bit x) )(list #x95 x)]
    [(list 'sta 'absy (16bit x) )(list #x99 x)]
    [(list 'sta 'absx (16bit x) )(list #x9D x)]

    ;LDA
    [(list 'lda 'zpxi (8bit x) )(list #xA1 x)]
    [(list 'lda 'zp   (8bit x) )(list #xA5 x)]
    [(list 'lda 'i    (8bit x) )(list #xA9 x)]
    [(list 'lda 'abs  (16bit x) )(list #xAD x)]
    [(list 'lda 'zpyi (8bit x) )(list #xB1 x)]
    [(list 'lda 'zpx  (8bit x) )(list #xB5 x)]
    [(list 'lda 'absy (16bit x) )(list #xB9 x)]
    [(list 'lda 'absx (16bit x) )(list #xBD x)]


    ;CMP
    [(list 'cmp 'zpxi (8bit x) )(list #xC1 x)]
    [(list 'cmp 'zp   (8bit x) )(list #xC5 x)]
    [(list 'cmp 'i    (8bit x) )(list #xC9 x)]
    [(list 'cmp 'abs  (16bit x) )(list #xCD x)]
    [(list 'cmp 'zpyi (8bit x) )(list #xD1 x)]
    [(list 'cmp 'zpx  (8bit x) )(list #xD5 x)]
    [(list 'cmp 'absy (16bit x) )(list #xD9 x)]
    [(list 'cmp 'absx (16bit x) )(list #xDD x)]

    ;SBC
    [(list 'sbc 'zpxi (8bit x) )(list #xE1 x)]
    [(list 'sbc 'zp   (8bit x) )(list #xE5 x)]
    [(list 'sbc 'i    (8bit x) )(list #xE9 x)]
    [(list 'sbc 'abs  (16bit x))(list #xED x)]
    [(list 'sbc 'zpyi (8bit x) )(list #xF1 x)]
    [(list 'sbc 'zpx  (8bit x) )(list #xF5 x)]
    [(list 'sbc 'absy (16bit x) )(list #xF9 x)]
    [(list 'sbc 'absx (16bit x) )(list #xFD x)]

    ; cc = 00
    
    ;ASL
    [(list 'asl 'zp   (8bit x) )(list #x06 x)]
    [(list 'asl 'i    #f          )(list #x0A)]
    [(list 'asl 'abs  (16bit x) )(list #x0E x)]
    [(list 'asl 'zpx  (8bit x) )(list #x16 x)]
    [(list 'asl 'absx (16bit x) )(list #x1E x)]

    ;ROL
    [(list 'rol 'zp   (8bit x) )(list #x26 x)]
    [(list 'rol 'a               )(list #x2A)]
    [(list 'rol 'abs  (16bit x) )(list #x2E x)]
    [(list 'rol 'zpx  (8bit x) )(list #x36 x)]
    [(list 'rol 'absx (16bit x) )(list #x3E x)]

    ;LSR
    [(list 'lsr 'zp   (8bit x) )(list #x46 x)]
    [(list 'lsr 'a              )(list #x4A)]
    [(list 'lsr 'abs  (16bit x) )(list #x4E x)]
    [(list 'lsr 'zpx  (8bit x) )(list #x56 x)]
    [(list 'lsr 'absx (16bit x) )(list #x5E x)]

    ;ROR
    [(list 'ror 'zp   (8bit x) )(list #x66 x)]
    [(list 'ror 'a              )(list #x6A)]
    [(list 'ror 'abs  (16bit x) )(list #x6E x)]
    [(list 'ror 'zpx  (8bit x) )(list #x76 x)]
    [(list 'ror 'absx (16bit x) )(list #x7E x)]

    ;STX
    [(list 'stx 'zp   (8bit x) )(list #x86 x)]
    [(list 'stx 'abs  (16bit x) )(list #x8E x)]
    [(list 'stx 'zpy  (8bit x) )(list #x96 x)]

    ;LDX
    [(list 'ldx 'i    (8bit x) )(list #xA2 x)]
    [(list 'ldx 'zp   (8bit x) )(list #xA6 x)]
    [(list 'ldx 'abs  (16bit x) )(list #xAE x)]
    [(list 'ldx 'zpx  (8bit x) )(list #xB6 x)]
    [(list 'ldx 'absy (16bit x) )(list #xBE x)]

    ;DEC
    [(list 'dec 'zp   (8bit x) )(list #xC6 x)]
    [(list 'dec 'abs  (16bit x) )(list #xCE x)]
    [(list 'dec 'zpx  (8bit x) )(list #xD6 x)]
    [(list 'dec 'absx (16bit x) )(list #xDE x)]

    ;INC
    [(list 'inc 'zp   (8bit x) )(list #xE6 x)]
    [(list 'inc 'abs  (16bit x) )(list #xEE x)]
    [(list 'inc 'zpx  (8bit x) )(list #xF6 x)]
    [(list 'inc 'absx (16bit x) )(list #xFE x)]

    ;BIT
    [(list 'bit 'zp   (8bit x) )(list #x24 x)]
    [(list 'bit 'abs  (16bit x) )(list #x2C x)]

    [(list 'jmp _   x )        (list #x4C (transition 'jump x))]

    ;(JMP)
    [(list 'jmpi 'abs (16bit x) )(list #x6C (transition 'jump x))]

    ;STY
    [(list 'sty 'zp   (8bit x) )(list #x84 x)]
    [(list 'sty 'abs  (16bit x) )(list #x8C x)]
    [(list 'sty 'zpx  (8bit x) )(list #x94 x)]

    ;LDY
    [(list 'ldy 'i    (8bit x) )(list #xA0 x)]
    [(list 'ldy 'zp   (8bit x) )(list #xA4 x)]
    [(list 'ldy 'abs  (16bit x) )(list #xAC x)]
    [(list 'ldy 'zpx  (8bit x) )(list #xB4 x)]
    [(list 'ldy 'absx (16bit x) )(list #xBC x)]

    ;CPY
    [(list 'cpy 'i    (8bit x) )(list #xC0 x)]
    [(list 'cpy 'zp   (8bit x) )(list #xC4 x)]
    [(list 'cpy 'abs  (16bit x))(list #xCC x)]

    ;CPX
    [(list 'cpx 'i    (8bit x) )(list #xE0 x)]
    [(list 'cpx 'zp   (8bit x) )(list #xE4 x)]
    [(list 'cpx 'abs  (16bit x))(list #xEC x)]

    ;Branches
    [(list 'bpl _      x        )(list #x10 (transition 'branch x))]
    [(list 'bmi _      x        )(list #x30 (transition 'branch x))]
    [(list 'bvc _      x        )(list #x50 (transition 'branch x))]
    [(list 'bvs _      x        )(list #x70 (transition 'branch x))]
    [(list 'bcc _      x        )(list #x90 (transition 'branch x))]
    [(list 'bcs _      x        )(list #xB0 (transition 'branch x))]
    [(list 'bne _      x        )(list #xD0 (transition 'branch x))]
    [(list 'beq _      x        )(list #xF0 (transition 'branch x))]

    ;Everything else
    [(list 'jsr _ x)             (list #x20 (transition 'jump x))]
    [(list 'rti _ _                 ) #x40]
    [(list 'rts _ _                 ) #x60]
    [(list 'php _ _                 ) #x08]
    [(list 'plp _ _                 ) #x28]
    [(list 'pha _ _                 ) #x48]
    [(list 'pla _ _                 ) #x68]
    [(list 'dey _ _                 ) #x88]
    [(list 'tay _ _                 ) #xA8]
    [(list 'iny _ _                 ) #xC8]
    [(list 'inx _ _                 ) #xE8]
    [(list 'clc _ _                 ) #x18]
    [(list 'sec _ _                 ) #x38]
    [(list 'cli _ _                 ) #x58]
    [(list 'sei _ _                 ) #x78]
    [(list 'tya _ _                 ) #x98]
    [(list 'clv _ _                 ) #xB8]
    [(list 'cld _ _                 ) #xD8]
    [(list 'sed _ _                 ) #xF8]
    [(list 'txa _ _                 ) #x8A]
    [(list 'txs _ _                 ) #x9A]
    [(list 'tax _ _                 ) #xAA]
    [(list 'tsx _ _                 ) #xBA]
    [(list 'dex _ _                 ) #xCA]
    [(list 'nop _ _                 ) #xEA])
  )


(define (infer-addressing-mode value is-immediate is-indirect register)
  (wdb "infer addressing mode ~a ~a ~a ~a" value is-immediate is-indirect register)
  (if (equal? value #f)
      'i ; special case for single opcodes with no operands      
      (let ([16bit?
             (or
              (and
               (symbol? value)
               (not (string-prefix? (symbol->string value) "<"))
               (not (string-prefix? (symbol->string value) ">")))
              (and (not (symbol? value)) (> value 255)))])
      
        (match (list 16bit? is-immediate is-indirect register)
          ;abs
          [(list #t #f #f 'x) 'absx]
          [(list #t #f #f 'y) 'absy]
          [(list #t #f #f _)  'abs ]

          ;zp
          ([list #f #f #f 'x] 'zpx)
          ([list #f #f #f 'y] 'zpy)
          ([list #f #f #f _]  'zp)

          ;immediate
          ([list #f #t _ _]  'i)

          ;indirect
          ([list #f #f #t 'x] 'zpxi)
          ([list #f #f #t 'y] 'zpyi)
          ([list #t #f #t _ ] 'jmpi)))))


(struct context (data location minl maxl jump-table labels-waiting branches-waiting breakpoints) #:mutable #:transparent)
(struct emulator (path program breakpoints? labels? execute?) #:mutable #:transparent)
(struct target-label (type relative location))
(define prog (context (make-vector 65536 #x0) 0 65536 0 (make-hash) (make-hash) (make-hash) (mutable-set)))
(define emu (emulator "" "" true true false))

(define (mon-commands-file)
  (string-append (emulator-program emu) ".mon"))

(define (execute-vice)
  (shell-execute
   #f
   (emulator-path emu)
   (format "-moncommands \"~a\" \"~a\"" (mon-commands-file) (emulator-program emu))
   (current-directory)
   'sw_shownormal))
    
(define (update-min v)
  (cond [(< v (context-minl prog)) (set-context-minl! prog v)]))

(define (update-max v)
  (cond [(> v (context-maxl prog)) (set-context-maxl! prog v)]))

(define (update-min-max v)
  (update-min v)
  (update-max v))

(define (set-location v)
  (set-context-location! prog v)
  (update-min-max v))

(define (inc-location)
  (set-location (+ 1 (context-location prog))))
 
(define (set-jump-source label location)
  (let* ([h (context-jump-table prog)]
         [v (hash-ref! h label '())])
    (hash-set! h label (cons location v))))
    
(define (set-jump-source-current label)
  (set-jump-source label (context-location prog)))

(define (add-jump-dest label type relative location)
  (wdb "adding jump dest ~a ~a ~a ~a" label type relative location)
  (let* ([h (context-labels-waiting prog)]
        [v (hash-ref! h label '())])
    (hash-set! h label (cons (target-label type relative location) v))))

(define (add-branch-dest label type relative location)
    (wdb "adding branch dest ~a ~a ~a ~a" label type relative location)
  (let* ([h (context-branches-waiting prog)]
        [v (hash-ref! h label '())])
    (hash-set! h label (cons (target-label type relative location) v))))

(define (set-current-value v)
  (vector-set! (context-data prog) (context-location prog) v))

(define (try-set-jump-source expr)
  (wdb "in try set jump source with ~a" expr)
  (cond [(symbol? expr)
         (wdb "setting jump source ~a" expr)
         (set-jump-source-current (symbol->string expr))]))

(define (write-transition-target branch? expr func)
  (wdb "write trans target ~a ~a " branch? expr)
  (let* ([s (symbol->string expr)]
         [type (cond
                 [(string-prefix? s "<") 'lo]
                 [(string-prefix? s ">") 'hi]
                 [else 'full])]
         [relative (cond
                     [(string-suffix? s "+") '+]
                     [(string-suffix? s "-") '-]
                     [else #f])])
    (let ([symbol-name
           (match (list type relative)
             [(list (or 'lo 'hi) (or '+ '-))
              (substring s 1 (- (string-length s) 1))]
             [(list (or 'lo 'hi) _)
              (wdb "in here ~a" (substring s 1))
              (substring s 1(- (string-length s) 1))]
             [(list _ (or '+ '-))
              (substring s 0 (- (string-length s) 1))]
             [_ (substring s 0 (- (string-length s) 1))]
             )])
      (wdb "transition target ~a ~a ~a" symbol-name type relative)
      (func (string-append ":" symbol-name) type relative (context-location prog))
      (inc-location)
      (when (and (equal? type 'full) (not branch?))
        (inc-location))
      (update-min-max (context-location prog))
      
      )))

(define (here) (context-location prog))

(define (write-value expr)
 ; (writeln (format "writing value ~a" expr))
  (cond [(symbol? expr)
         (write-transition-target #f expr add-jump-dest)]
        [(number? expr)
         (begin
           (set-current-value expr)
           (inc-location)
           
           (update-min-max (context-location prog)))]))

(define (write-values exprs)
  (for ([e (flatten exprs)])
    (match e
      [(transition 'branch label)
       (if (number? label)
           (write-value (lo-byte (-  label (context-location prog))))
           (write-transition-target #t label add-branch-dest))]
      [(transition 'jump label)
       (if (number? label)
           (begin
             (write-value (lo-byte label))
             (write-value (hi-byte label)))             
           (write-transition-target #f label add-jump-dest))]
      [_
       (write-value e)])))


(define (process-line inputs)
  (match-let ([(list source-label opcode target indirect immediate register) inputs])
    (begin
      (wdb "process-line ~a ~a ~a ~a ~a ~a" source-label opcode target indirect immediate register)
      (try-set-jump-source source-label)
      (let ([addressing-mode (infer-addressing-mode target immediate indirect register)])
        (to-bytes (list opcode addressing-mode target))))))
               

(begin-for-syntax
  (define-syntax-class label-targ
    (pattern x:id #:when
             (let ([s (symbol->string (syntax-e #'x))])
               (or (string-suffix? s ":")
                   (string-suffix? s "+")
                   (string-suffix? s "-"))))))

(begin-for-syntax
  (define-syntax-class label
    (pattern x:id #:when
             (let ([s (symbol->string (syntax-e #'x))])
               (or (string-prefix? s ":"))))))


(define-syntax (label-loc stx)
  (syntax-parse stx
    [(_ label:label-targ)
     (let ([s (symbol->string (syntax-e #'label))])
       (with-syntax ([new-symbol (string-append ":" (substring s 0 (- (string-length s) 1)))])
       (cond
         [(string-suffix? s ":")
            #'(find-closest-label 'new-symbol (here) #f)]
         [(string-suffix? s "+")
            #'(find-closest-label 'new-symbol (here) '+)]
         [(string-suffix? s "-")
            #'(find-closest-label 'new-symbol (here) '-)]

         
         )
         
         )


     )]))
          
        


(define-syntax (expand-line stx)
; (writeln stx)
  (begin
    (syntax-parse stx
    [(_ lab (~literal *=) t:nat _ _ _ )
     #'(set-location t)]

    [(_ lab op (~or p:label-targ p:nat) imm ind reg)
     #'(write-values (process-line (list 'lab 'op 'p ind imm 'reg)))]
            
    [(_ lab #f p:expr _ _ _)
     ; this case is an expression with no opcode, so we let it pass through

     ; but stil allow for a label
     (begin       
       #'(begin
           (try-set-jump-source `lab)
           p))]
    
    [(_ lab op p:expr imm ind reg)
     #'(write-values (process-line (list 'lab 'op  p ind imm 'reg)))])))

(define-syntax (6502-line stx)  
  (define-syntax-class immediate
    (pattern #:immediate))

  (define-syntax-class indirect
    (pattern #:indirect))

  (define-syntax-class register
    (pattern (~or (~literal x) (~literal y))))

  (syntax-parse stx
    [(_ label:label)
     #'(try-set-jump-source `label)]
    [(_ label:label e:expr)
        #'(begin (try-set-jump-source `label) e) ]
    [(_ (~seq
         (~optional label:label #:defaults ([label #'#f]))
         oc:id
         (~optional ind:indirect #:defaults ([ind #'#f]))
         (~optional imm:immediate #:defaults ([imm #'#f]))
         (~optional (~or targ:label-targ targ:id targ:number targ:expr) #:defaults ([targ #'#f]))
         (~optional reg:register #:defaults ([reg #'#f]))))
                    
     #'(begin
         (expand-line
          label oc targ
          (equal? `#:immediate `imm)
          (equal? `#:indirect `ind)
          reg))]
        
    [(_ e:expr) #'e]))

(define-syntax (6502-block stx)
  (syntax-parse stx
    ([_ line ... ] #'(begin line ... ))))

(define-syntax (data stx)
  (syntax-parse stx
    [(_ v ... )
     #'(write-values (list v ...))]))

(define-for-syntax (extract-immediate-args arg-symbols arg-names)
  (define (aux inputs immediates all-args i)
    (match inputs
      [(list-rest #:immediate a tail)
       (aux tail (cons (list-ref arg-names i) immediates) (cons a all-args) (+ i 1))]
      [(list-rest a tail) (aux tail immediates (cons a all-args) (+ i 1))]
      ['() (values immediates (reverse all-args))]))
  (aux arg-symbols '() '() 0))

(define-for-syntax (index-of needle haystack)
  (define (aux input index)
    (match input
      [(list-rest a tail) #:when (equal? a needle) index]
      [(list-rest a tail) (aux tail (+ index 1))]))
  (aux haystack 0))
                  
(define-syntax (define-op stx)
  (syntax-parse stx
    [(_ (name args ...) body)
     (let ([arg-names (map syntax-e (syntax->list #'(args ...)))])
       (with-syntax ([new-arg-names arg-names])
       #'(define-syntax (name stx)
           (syntax-parse stx
             [(_ iargs (... ...))
              (let*-values
                  ([(args/sym) (map syntax-e (syntax->list #'(iargs (... ...))))]
                   [(names) (syntax->datum #'new-arg-names)]
                   [(immediates all-args) (extract-immediate-args args/sym names)])
                (define (expr n)
                  (list '#:immediate n))
                (define (aux input output needs-imm)
                  (match input
                    [(list-rest a tail) #:when (list? a)
                     (begin
                       (define-values (nested nested-imm)
                         (aux a '() needs-imm))
                       (aux tail (cons (reverse nested) output) nested-imm))]
                    [(list-rest a tail) #:when (member a immediates)
                     (define-values (next needs)
                       (aux tail (cons (list-ref all-args (index-of a names)) output) needs-imm))
                     (values next #t)]

                    [(list-rest a tail) #:when (member a names)
                     (aux tail (cons (list-ref all-args (index-of a names)) output) needs-imm)]
                    [(list-rest a tail) #:when (equal? a '6502-line)
                                  (define-values (next needs)
                       (aux tail (cons a output) needs-imm))
                     (define new-yay
                       (let ([x (car next)])
                         (cons x (cons '#:immediate (cdr next)))))
                     (if needs
                         (values new-yay #f)
                         (values next needs))
                     ]
                    [(list-rest a tail)
                     (aux tail (cons a output) needs-imm)]
                    ['() ;(writeln (format "end ~a" needs-imm))
                     (values output needs-imm)]))
                (define-values (new-body ignore)
                  (aux (syntax->datum #'body) '() #f)
                  )
                (define (create-bool name val)
                  (define is-imm (not (equal? (member name immediates) #f)))

                  (define fmt (format-id stx "~a-immediate?" name))
                  (define fmt-16 (format-id stx "~a-16bit?" name))
                  (list (datum->syntax stx (list fmt is-imm))
                        (datum->syntax stx (list fmt-16 (list '> val 255)))))
                (with-syntax
                  ([arg-bools (datum->syntax stx (flatten (map create-bool names all-args)))]
                   [new-body (datum->syntax stx (reverse new-body))])
                  #'(let arg-bools
                          new-body)
                  ))]))))]))
            
(define (find-closest-label key location relative)
  (define (aux input)
    (let-values ([(input f)
                  (if (equal? relative '+)
                      (values (sort input <) >)
                      (values (sort input >) <))])
      (match input
        [(list-rest a _) #:when (f a location) a]
        [(list-rest a tail) (aux tail)])))
  (let ([labels (hash-ref (context-jump-table prog) key)])
    (wdb "searching labels ~A from ~a ~a" labels location relative)
    (if (= 1 (length labels))
        (car labels)
        (aux labels))))

(define-syntax (if-immediate stx)
  (syntax-parse stx
    [(_ (#:immediate _) true-branch false-branch) #'true-branch]
    [(_ _ true-branch false-branch) #'false-branch]))

(define-syntax (C64 stx)
  (syntax-parse stx
    [(_ a ...)
     #'(begin
         a ...
         (hash-for-each
          (context-labels-waiting prog)
          (λ (k dest)
            (for [(current-target dest)]
              (let ([actual
                     (find-closest-label                      
                      k
                      (target-label-location current-target)
                      (target-label-relative current-target))])
                (case (target-label-type current-target)
                   ['full
                    (begin
                      (vector-set!
                       (context-data prog)
                       (target-label-location current-target)
                       (lo-byte actual))
                      (vector-set!
                       (context-data prog)
                       (+ 1 (target-label-location current-target))
                       (hi-byte actual)))]
                   ['hi
                    (vector-set!
                     (context-data prog)
                     (target-label-location current-target)
                     (hi-byte actual))]
                   ['lo
                    (vector-set!
                     (context-data prog)
                     (target-label-location current-target)
                     (lo-byte actual))])))))
 
         (hash-for-each
          (context-branches-waiting prog)
          (λ (k dest)
            (for [(current-target dest)]
              (let ([actual
                     (find-closest-label
                      k
                      (target-label-location current-target)
                      (target-label-relative current-target))])
                (wdb "writing branch dest ~a ~a" k actual)
                (vector-set!
                 (context-data prog)
                 (target-label-location current-target)
                 (lo-byte (- actual (target-label-location current-target) 1)))))))

         ;write numbers to file!
         (define out (open-output-file (emulator-program emu) #:exists 'replace #:mode 'binary))
         (write-byte (lo-byte (context-minl prog)) out)
         (write-byte (hi-byte (context-minl prog)) out)
         (for ([i  (vector-copy (context-data prog)(context-minl prog) (context-maxl prog) )])
           (write-byte i out))
         (close-output-port out)
         (unless (not (emulator-execute? emu))
             (begin
               (let ([out (open-output-file (mon-commands-file) #:exists 'replace)])
                 (when (emulator-breakpoints? emu)
                   (set-for-each
                    (context-breakpoints prog)
                    (λ (loc)
                      (write-string (format "break ~a\n" (number->string loc 16)) out))))
                 (when (emulator-labels? emu)
                   
                   (hash-for-each
                    (context-jump-table prog)
                    (λ (k dests)
                      (begin
                        (for ([dest dests])
                          (write-string (format "al ~a .~a\n" (number->string dest 16) k) out)))))
                   (close-output-port out))
                 (execute-vice)))))]))
  

(provide (all-defined-out))

