;Asi64
;Copyright Ross McKinlay, 2017

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
(struct metadata (opcode          ; opcode symbol
                  addressing-mode ; symbol
                  value           ; actual bin representation
                  operand-size    ; 1, 2 (8 bit), 3 (16 bit)
                  flags-affected  ; list
                  cycle-count     ; base cycle cost
                  page-penalty?   ; +1 cycle if crossing page?
                  transition-type ; 'none 'branch or 'jump
                  ) #:transparent)

(define-syntax (create-opcode-metadata stx)
  (syntax-parse stx
    [(_ ([opcode
          addressing-mode
          value
          size
          (flag ...)
          cycles
          page-penalty?
          transition-type] ...))
     #'(make-hash
        (list
         (cons
          (cons opcode addressing-mode)
          (metadata
           opcode
           addressing-mode
           value
           size
           (list flag ...)
           cycles
           page-penalty?
           transition-type)) ...))]))

(define opcode-metadata
  (create-opcode-metadata
   (['ora 'zpxi #x01 2 () 1 #f 'none] 
    ['ora 'zp   #x05 2 () 1 #f 'none] 
    ['ora 'i    #x09 2 () 1 #f 'none] 
    ['ora 'abs  #x0D 3 () 1 #f 'none] 
    ['ora 'zpyi #x11 2 () 1 #f 'none] 
    ['ora 'zpx  #x15 2 () 1 #f 'none] 
    ['ora 'absy #x19 3 () 1 #f 'none] 
    ['ora 'absx #x1D 3 () 1 #f 'none] 

    ; AND
    ['and 'zpxi #x21 2 () 1 #f 'none] 
    ['and 'zp   #x25 2 () 1 #f 'none] 
    ['and 'i    #x29 2 () 1 #f 'none] 
    ['and 'abs  #x2D 3 () 1 #f 'none] 
    ['and 'zpyi #x31 2 () 1 #f 'none] 
    ['and 'zpx  #x35 2 () 1 #f 'none] 
    ['and 'absy #x39 3 () 1 #f 'none] 
    ['and 'absx #x3D 3 () 1 #f 'none] 

    ;EOR
    ['eor 'zpxi #x41 2 () 1 #f 'none] 
    ['eor 'zp   #x45 2 () 1 #f 'none] 
    ['eor 'i    #x49 2 () 1 #f 'none] 
    ['eor 'abs  #x4D 3 () 1 #f 'none] 
    ['eor 'zpyi #x51 2 () 1 #f 'none] 
    ['eor 'zpx  #x55 2 () 1 #f 'none] 
    ['eor 'absy #x59 3 () 1 #f 'none] 
    ['eor 'absx #x5D 3 () 1 #f 'none] 

    ;ADC
    ['adc 'zpxi #x61 2 () 1 #f 'none] 
    ['adc 'zp   #x65 2 () 1 #f 'none] 
    ['adc 'i    #x69 2 () 1 #f 'none] 
    ['adc 'abs  #x6D 3 () 1 #f 'none]     
    ['adc 'zpyi #x71 2 () 1 #f 'none] 
    ['adc 'zpx  #x75 2 () 1 #f 'none] 
    ['adc 'absy #x79 3 () 1 #f 'none] 
    ['adc 'absx #x7D 3 () 1 #f 'none] 

    ;STA
    ['sta 'zpxi #x81 2 () 1 #f 'none] 
    ['sta 'zp   #x85 2 () 1 #f 'none] 
    ['sta 'abs  #x8D 3 () 1 #f 'none] 
    ['sta 'zpyi #x91 2 () 1 #f 'none] 
    ['sta 'zpx  #x95 2 () 1 #f 'none] 
    ['sta 'absy #x99 3 () 1 #f 'none] 
    ['sta 'absx #x9D 3 () 1 #f 'none] 

    ;LDA
    ['lda 'zpxi #xA1 2 () 1 #f 'none] 
    ['lda 'zp   #xA5 2 () 1 #f 'none] 
    ['lda 'i    #xA9 2 () 1 #f 'none] 
    ['lda 'abs  #xAD 3 () 1 #f 'none] 
    ['lda 'zpyi #xB1 2 () 1 #f 'none] 
    ['lda 'zpx  #xB5 2 () 1 #f 'none] 
    ['lda 'absy #xB9 3 () 1 #f 'none] 
    ['lda 'absx #xBD 3 () 1 #f 'none] 

    ;CMP
    ['cmp 'zpxi #xC1 2 () 1 #f 'none] 
    ['cmp 'zp   #xC5 2 () 1 #f 'none] 
    ['cmp 'i    #xC9 2 () 1 #f 'none] 
    ['cmp 'abs  #xCD 3 () 1 #f 'none] 
    ['cmp 'zpyi #xD1 2 () 1 #f 'none] 
    ['cmp 'zpx  #xD5 2 () 1 #f 'none] 
    ['cmp 'absy #xD9 3 () 1 #f 'none] 
    ['cmp 'absx #xDD 3 () 1 #f 'none] 

    ;SBC
    ['sbc 'zpxi #xE1 2 () 1 #f 'none] 
    ['sbc 'zp   #xE5 2 () 1 #f 'none] 
    ['sbc 'i    #xE9 2 () 1 #f 'none] 
    ['sbc 'abs  #xED 3 () 1 #f 'none] 
    ['sbc 'zpyi #xF1 2 () 1 #f 'none] 
    ['sbc 'zpx  #xF5 2 () 1 #f 'none] 
    ['sbc 'absy #xF9 3 () 1 #f 'none] 
    ['sbc 'absx #xFD 3 () 1 #f 'none] 

    ;ASL
    ['asl 'zp   #x06 2 () 1 #f 'none] 
    ['asl 'none #x0A 1 () 1 #f 'none]
    ['asl 'abs  #x0E 3 () 1 #f 'none] 
    ['asl 'zpx  #x16 2 () 1 #f 'none] 
    ['asl 'absx #x1E 3 () 1 #f 'none] 

    ;ROL
    ['rol 'zp   #x26 2 () 1 #f 'none] 
    ['rol 'none #x2A 1 () 1 #f 'none]
    ['rol 'abs  #x2E 3 () 1 #f 'none] 
    ['rol 'zpx  #x36 2 () 1 #f 'none] 
    ['rol 'absx #x3E 3 () 1 #f 'none] 

    ;LSR
    ['lsr 'zp   #x46 2 () 1 #f 'none] 
    ['lsr 'none #x4A 1 () 1 #f 'none]
    ['lsr 'abs  #x4E 3 () 1 #f 'none] 
    ['lsr 'zpx  #x56 2 () 1 #f 'none] 
    ['lsr 'absx #x5E 3 () 1 #f 'none] 

    ;ROR
    ['ror 'zp   #x66 2 () 1 #f 'none] 
    ['ror 'none #x6A 1 () 1 #f 'none]
    ['ror 'abs  #x6E 3 () 1 #f 'none] 
    ['ror 'zpx  #x76 2 () 1 #f 'none] 
    ['ror 'absx #x7E 3 () 1 #f 'none] 

    ;STX
    ['stx 'zp   #x86 2 () 1 #f 'none] 
    ['stx 'abs  #x8E 3 () 1 #f 'none] 
    ['stx 'zpy  #x96 2 () 1 #f 'none] 

    ;LDX
    ['ldx 'i    #xA2 2 () 1 #f 'none] 
    ['ldx 'zp   #xA6 2 () 1 #f 'none] 
    ['ldx 'abs  #xAE 3 () 1 #f 'none] 
    ['ldx 'zpx  #xB6 2 () 1 #f 'none] 
    ['ldx 'absy #xBE 3 () 1 #f 'none] 

    ;DEC
    ['dec 'zp   #xC6 2 () 1 #f 'none] 
    ['dec 'abs  #xCE 3 () 1 #f 'none] 
    ['dec 'zpx  #xD6 2 () 1 #f 'none] 
    ['dec 'absx #xDE 3 () 1 #f 'none] 

    ;INC
    ['inc 'zp   #xE6 2 () 1 #f 'none] 
    ['inc 'abs  #xEE 3 () 1 #f 'none] 
    ['inc 'zpx  #xF6 2 () 1 #f 'none] 
    ['inc 'absx #xFE 3 () 1 #f 'none] 

    ;BIT
    ['bit 'zp   #x24 2 () 1 #f 'none] 
    ['bit 'abs  #x2C 3 () 1 #f 'none] 

    ;STY
    ['sty 'zp   #x84 2 () 1 #f 'none] 
    ['sty 'abs  #x8C 3 () 1 #f 'none] 
    ['sty 'zpx  #x94 2 () 1 #f 'none] 

    ;LDY
    ['ldy 'i    #xA0 2 () 1 #f 'none] 
    ['ldy 'zp   #xA4 2 () 1 #f 'none] 
    ['ldy 'abs  #xAC 3 () 1 #f 'none] 
    ['ldy 'zpx  #xB4 2 () 1 #f 'none] 
    ['ldy 'absx #xBC 3 () 1 #f 'none] 

    ;CPY
    ['cpy 'i    #xC0 2 () 1 #f 'none] 
    ['cpy 'zp   #xC4 2 () 1 #f 'none] 
    ['cpy 'abs  #xCC 3 () 1 #f 'none] 

    ;CPX
    ['cpx 'i    #xE0 2 () 1 #f 'none] 
    ['cpx 'zp   #xE4 2 () 1 #f 'none] 
    ['cpx 'abs  #xEC 3 () 1 #f 'none] 

    ;JMP / JSR
    ['jmp 'jmpi #x6C 3 () 1 #f 'jump]
    ['jmp 'abs  #x4C 3 () 1 #f 'jump]
    ['jsr 'abs  #x20 3 () 1 #f 'none]
    
    ;Branches
    ;these are actually "relative" addressing mode,
    ;but that is not a thing in asi64. You must give it
    ;either a label or a 16 bit address and it works out
    ;the relative addressing - hence the ;'abs for these.
    ['bpl 'abs #x10 2 () 1 #f 'branch] 
    ['bmi 'abs #x30 2 () 1 #f 'branch]
    ['bvc 'abs #x50 2 () 1 #f 'branch]
    ['bvs 'abs #x70 2 () 1 #f 'branch]
    ['bcc 'abs #x90 2 () 1 #f 'branch]
    ['bcs 'abs #xB0 2 () 1 #f 'branch]
    ['bne 'abs #xD0 2 () 1 #f 'branch]
    ['beq 'abs #xF0 2 () 1 #f 'branch]

    ;Everything else
    ['rti 'none #x40 1 () 1 #f 'none]
    ['rts 'none #x60 1 () 1 #f 'none]
    ['php 'none #x08 1 () 1 #f 'none]
    ['plp 'none #x28 1 () 1 #f 'none]
    ['pha 'none #x48 1 () 1 #f 'none]
    ['pla 'none #x68 1 () 1 #f 'none]
    ['dey 'none #x88 1 () 1 #f 'none]
    ['tay 'none #xA8 1 () 1 #f 'none]
    ['iny 'none #xC8 1 () 1 #f 'none]
    ['inx 'none #xE8 1 () 1 #f 'none]
    ['clc 'none #x18 1 () 1 #f 'none]
    ['sec 'none #x38 1 () 1 #f 'none]
    ['cli 'none #x58 1 () 1 #f 'none]
    ['sei 'none #x78 1 () 1 #f 'none]
    ['tya 'none #x98 1 () 1 #f 'none]
    ['clv 'none #xB8 1 () 1 #f 'none]
    ['cld 'none #xD8 1 () 1 #f 'none]
    ['sed 'none #xF8 1 () 1 #f 'none]
    ['txa 'none #x8A 1 () 1 #f 'none]
    ['txs 'none #x9A 1 () 1 #f 'none]
    ['tax 'none #xAA 1 () 1 #f 'none]
    ['tsx 'none #xBA 1 () 1 #f 'none]
    ['dex 'none #xCA 1 () 1 #f 'none]
    ['nop 'none #xEA 1 () 1 #f 'none])))


(define (to-bytes input)
  (match-let* ([(list opcode a-mode operand) input]
               [(metadata _ _ v s _ _ _ tt)                 
                (hash-ref opcode-metadata (cons opcode a-mode))])
    (match (list tt s operand)
       [(list 'none 1 op)         (list v)]
       [(list 'none 2 (8bit op))  (list v op)]
       [(list 'none 3 (16bit op)) (list v op)]
       [(list tt _ op)            (list v (transition tt op))])))

(define (infer-addressing-mode value is-immediate is-indirect register)
  (wdb "infer addressing mode ~a ~a ~a ~a" value is-immediate is-indirect register)
  (if (equal? value #f)
      'none ; special case for single opcodes with no operands      
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
(struct target-label (type relative location) #:transparent)
(define prog (context (make-vector 65536 #x0) 0 65536 0 (make-hash) (make-hash) (make-hash) (mutable-set)))
(define emu (emulator "" "" true true false))

(define (configure-emu emu-path program-path execute-emu? enable-breakpoints?)
  (set-emulator-program! emu program-path)
  (set-emulator-execute?! emu execute-emu?)
  (set-emulator-breakpoints?! emu enable-breakpoints?)
  (set-emulator-path! emu emu-path))

(define (mon-commands-file)
  (string-append (emulator-program emu) ".mon"))

(define (execute-vice)
  (if (eq? (system-type 'os) 'windows)
      (shell-execute
       #f
       (emulator-path emu)
       (format "-moncommands \"~a\" \"~a\"" (mon-commands-file) (emulator-program emu))
       (current-directory)
       'sw_shownormal)   
      (system      
       (format "\"~a\" -moncommands \"~a\" \"~a\"" (emulator-path emu) (mon-commands-file) (emulator-program emu))
       )))
    
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

(define (set-jump-source-next label)
  (set-jump-source label (+ (context-location prog) 1)))

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

(define (try-set-jump-source expr f)
  (wdb "in try set jump source with ~a" expr)
  (cond [(symbol? expr)
         (wdb "setting jump source ~a" expr)
         (f (symbol->string expr))]))

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

(define (align n)
  (define (aux i)
    (if (eq? (remainder i n) 0)
        (set-location i)
        (aux (+ i 1))))
  (aux (context-location prog)))

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
  (match-let ([(list source-label source-label2 opcode target indirect immediate register) inputs])
    (begin
      (wdb "process-line ~a ~a ~a ~a ~a ~a" source-label opcode target indirect immediate register)
      (let ([addressing-mode (infer-addressing-mode target immediate indirect register)])
        ; special case here to check for the 6502 bug where you can't use an indirect jump
        ; at the end of a page.
        (when (and (eq? opcode 'jmp)
                   (eq? addressing-mode 'jmpi)
                   (number? target)
                   (eq? (lo-byte target) 255))
          (writeln (format "warning: indirect jump target on page boundary at $~x!" (here))))
        (if (eq? opcode 'break) ;special "opcode" for emu breakpoints
            (set-add! (context-breakpoints prog) (context-location prog))
            (to-bytes (list opcode addressing-mode target)))))))

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
         )))]))
          
         
(define-syntax (expand-line stx)
; (writeln stx)
  (begin
    (syntax-parse stx
    [(_ lab lab2 (~literal *=) t:nat _ _ _ )
     #'(set-location t)]

    [(_ lab lab2 (~literal /=) t:nat _ _ _ )
     #'(align t)]

    [(_ lab lab2 op (~or p:label-targ p:nat) imm ind reg)
     #'(begin
         (try-set-jump-source 'lab set-jump-source-current)
         (try-set-jump-source 'lab2 set-jump-source-next)
         (write-values (process-line (list 'lab 'lab2 'op 'p ind imm 'reg))) )]
            
    [(_ lab lab2 #f p:expr _ _ _)
     ; this case is an expression with no opcode, so we let it pass through
     ; but stil allow for a label
     (begin       
       #'(begin
           (try-set-jump-source `lab)
           p))]
    
    [(_ lab lab2 op p:expr imm ind reg)
     #'(begin
         (try-set-jump-source 'lab set-jump-source-current)
         (try-set-jump-source 'lab2 set-jump-source-next)
         (write-values (process-line (list 'lab 'lab2 'op  p ind imm 'reg))))])))

(define-syntax (6502-line stx)  
  (define-syntax-class immediate
    (pattern #:immediate))

  (define-syntax-class indirect
    (pattern #:indirect))

  (define-syntax-class register
    (pattern (~or (~literal x) (~literal y))))

  (syntax-parse stx #:datum-literals (=)
    [(_ label:label)
     #'(try-set-jump-source `label set-jump-source-current)]
    [(_ label:label e:expr)
     #'(begin (try-set-jump-source `label set-jump-source-current) e) ]
    [(_ v:identifier = e:expr)
     #'(define v e)]
    [(_ (~seq
         (~optional label:label #:defaults ([label #'#f]))
         oc:id
         (~optional label2:label #:defaults ([label2 #'#f]))
         (~optional ind:indirect #:defaults ([ind #'#f]))
         (~optional imm:immediate #:defaults ([imm #'#f]))
         (~optional (~or targ:label-targ targ:id targ:number targ:expr) #:defaults ([targ #'#f]))
         (~optional reg:register #:defaults ([reg #'#f]))))
     #'(begin
         (expand-line
          label label2 oc targ
          (equal? `#:immediate `imm)
          (equal? `#:indirect `ind)
          reg))]
        
    [(_ e:expr ... ) #'(begin e ...) ]
    [(_ e ) #'e] ))

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

;; i have no idea what I am doing, this is probably the worst!
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
                  (define is-label (and (symbol? val)  (string-suffix? (symbol->string val) ":")))
                  (define fmt (format-id stx "~a-immediate?" name))
                  (define fmt-16 (format-id stx "~a-16bit?" name))
                  (define is-16 (if is-label #f (list '> val 255))) 
                  (list (datum->syntax stx (list fmt is-imm))
                        (datum->syntax stx (list fmt-16 is-16))))
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
    (wdb "searching labels ~A for ~a from ~a ~a" labels key location relative)
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
                ; if this is an indirect jump, emit a warning if the lo byte is on the end of
                ; a page. here we check the previous byte for 6C which is indirect jump.
                (when
                  (and
                   (eq? (vector-ref
                         (context-data prog)
                         (- (target-label-location current-target) 1)) #x6C)
                   (eq? (lo-byte actual) 255))
                  (writeln (format "warning: indirect jump target on page boundary at $~x ~a"
                                   (target-label-location current-target) k)))
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
              (let*
                  ; find the label
                  ([actual  
                    (find-closest-label
                      k
                      (target-label-location current-target)
                      (target-label-relative current-target))]
                   ; calculate offset in bytes
                   [amount (- actual (target-label-location current-target) 1)])

                (when (or (> amount 127) (< amount -127))
                  (writeln
                   (format "warning: attempted to branch over +/-127 (~a) bytes to label ~a from location $~x"
                            amount k (target-label-location current-target))))

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

         (when (emulator-execute? emu)
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
