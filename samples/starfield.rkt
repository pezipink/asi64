#lang asi64
(require (for-syntax syntax/parse))
(set-emulator-program! emu "c64.prg")
(set-emulator-execute?! emu #f)
(set-emulator-breakpoints?! emu #t) ;bps
(set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")

(define vic-control $d018)
(define black 0)
(define white 1)
(define background $d021)
(define border $d020)

(define-op (mov src dst){
	lda src
	sta dst
})

(define-syntax (fori stx)
  (syntax-parse stx
    ([_ e f]
     ; breaking hygeine here, very sorry
     (with-syntax ([f2 (syntax->datum #'f)])
      #'(for ([i (in-range 0 e)])
                 f2)))))

(define (clear-screen start character)
  {	ldx @0
        lda @character            
 :loop	(for ([i '(0 1 2 3)])
           {sta (+ start (* i $100)) x})
       	dex
	bne loop-   })


(struct starbank (location len rows))

(define (generate-starfield-updates starbank going-right?)
  (define (char-index i)
    (+ (starbank-location starbank)  (* 8 i)))

  (for ([row (starbank-rows starbank)])
    (let ([row (car row)]
          [speed (cadr row)])
      (for ([s (in-range 0 speed)])        
        {clc 
         (for ([char (if going-right?
                         (in-range 0 (starbank-len starbank))
                         (in-range (- (starbank-len starbank) 1) -1 -1))])
           (if going-right?
               {ror (+ (char-index char) row)}
               {rol (+ (char-index char) row)}))

         ;finally wrap around if the carry is et
         bcc end+
         (if going-right?
             {ror (+ (char-index 0) row)}
             {rol (+ (char-index (- (starbank-len starbank) 1)) row)})
         :end}))))



(C64{
        *= $0c00 ;asssemble at video memory location
        (let ([perm1 '(0 1 2 3 4)]
              [perm2 '(4 0 1 2 3)]
              [perm3 '(3 4 0 1 2)]
              [perm4 '(2 3 4 0 1)]
              [order '(1 4 2 1 3 1 2 4
                         1 4 2 1 3 1 2 4
                         1 4 2 1 3 1 2 4
                         1)])
          (for ([next order])
            (for([count (in-range 8)])
              (case next
                [(1) (data perm1)]
                [(2) (data perm2)]
                [(3) (data perm3)]
                [(4) (data perm4)]))))                   
           
	*= $1000
        ;screen at $0c00 charset at $2000
	(mov @%00111000 vic-control)
	(mov @black background)
      	(mov @black border)
        (clear-screen $d800 1)
        jsr create_chars+
        (define sb1 (starbank $2000 5 '((1 3) (3 1) (5 2))))
        lda @$ff
:loop 	cmp $d012 ; wait for line 256
        bne loop-
        jsr update_starfield+
        ; do nothing for a while so 
        ; this doesn't trigger more than
        ; once on the same frame!
        (fori 100 {nop})
        lda @$ff
        jmp loop-

:update_starfield
        (generate-starfield-updates sb1 #t)
        rts

:create_chars
        lda @0
	;splat the first 5 characters 
	(fori (* 8 5) {sta (+ $2000 i)})
        (mov @%00100000 $2001)
	(mov @%00001000 $2003)
        (mov @%00000010 $2005)
        rts
        

})

