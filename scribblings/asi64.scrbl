#lang scribble/manual

@(require (for-label racket))

@title{Asi64}

@italic{Written by @hyperlink["http://www.pinksquirrellabs.com"]{Ross McKinlay}}

The Racket based cross-platform 6502  assembler.  Primarily aimed at programming the Commodore 64, with @hyperlink["http://vice-emu.sourceforge.net/"]{VICE emulator} support.

@table-of-contents[]

@section[#:tag "overview"]{Asi64 Overview}

The overall philosophy of this project is to take an old idea (a 6502 macro assembler) and supercharge it with Racket's macro system.  The end result is a powerful combination of part assembler, part meta-programming language that lets you combine all the power of Racket with a raw but feature-rich assembler.

Since Racket embraces this manner of design, it is quite easy to change, extend, or even build new languages over the top of it.

The assembler is primarily aimed at programming the Commodore 64, and includes support for the emulator @hyperlink["http://vice-emu.sourceforge.net/"]{VICE}. It is able to launch the emulator post-compilation, passing along both your breakpoints and labels, resulting in a fast development cycle.

An assembler is a powerful low-level tool. An overall choice was made to expose all (or almost all) of the internals of the assembler to the programmer.  The special assembler syntax provided is, afterall, simply a wrapper around the underlying library, consisting of some macros, functions and read-table modifications.  The programmer is able to inspect and modify the assembler internals as they please. This can make it easier to write complex macros that directly program the internals without having to jump through hoops using the provided layers of syntax.

The code can be found at the @hyperlink["https://github.com/pezipink/asi64"]{git repository found here}.

@section{Getting Started}

You can find asi64 on Racket's package manager. @tt{raco pkg install asi64} should get you setup and ready to go. Create a Racket file somewhere for your program, and have @hyperlink["http://vice-emu.sourceforge.net/"]{VICE} installed and ready to go.  
@defmodulelang[asi64]

@#reader scribble/comment-reader
(racketblock

;setup emulator             
(set-emulator-program! emu "c64.prg")
(set-emulator-execute?! emu #t)
(if (eq? (system-type 'os) 'windows)
  (set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")
  (set-emulator-path! emu "/snap/bin/vice-jz.x64"))

;main program
(C64 {
      *= $1000   ;start at $1000
:loop inc $d021  ;flash border
      jmp loop-  ;loop forever
code:blank
})
)

The first few lines tell the assembler where to create the output file, whether to start the emulator post-assembly, and the location of the emulator itself.  See @secref{emulator configuration} for more information.

An asi64 program must contain a single core form @tt{C64}.  Within this form is expected to be a single 6502 block, denoted by @tt{ { }}.

If you compile this program, it should assemble the resulting file and start the emulator with it for you to enjoy (hopefully!)

@section{Core Assembler Syntax}

@subsection{General}

Asi64 tries to stay as close to traditional 6502 assembler as possible. However, a few changes had to be made to ensure all of Racket is available.

You can write 6502 assembler code anywhere between @tt{{ }}.  These blocks can be mixed and nested arbitrarily with other Racket forms - see @secref{racket integration} for more infromation.

Within the 6502 blocks, each opcode must appear on its own line.  Operands can be formed of literals or Racket expressions, and labels can appear in a variety of places, see the @secref{labels} section for more information.

@subsection{Number Literals}

Asi64 provides hexadecimal @tt{$} and binary @tt{%} literals, which  can be used everywhere in an asi64 program, not just within 6502 blocks.

@#reader scribble/comment-reader
(racketblock
{
   lda \@%1111_0000   ; binary 
   sta $d020         ; hex   
code:blank
} 
)


Binary literals are able to have any amount of underscore characters within them, to help visually separate groups of bits.

@subsection{Addressing Modes}

Immediate addressing mode is traditionally @tt{#}.  Since @tt{#} is already used for things in Racket, asi64 instead uses @tt{@literal|{@}|}.  Therefore, @tt{lda #42} becomes @tt{lda @literal|{@}|42}

Indexed adrressing modes are almost the same, removing the comma.  @tt{sta $2000,x}  becomes @tt{sta $2000 x}.

Finally, indirect addressing modes are quite different, since the traditional parenthesis would have caused havoc.  Instread, prefix the operand with £, and remove the comma where applicable.  @tt{sta ($20),y} becomes @tt{sta £ $20 y}  and @tt{jmp ($4000)} becomes @tt{jmp £ $4000}.

If the @tt{£} character is not on your keyboard, you can easily change it to something else by modifiying @tt{"reader.rkt"}.

@subsection{Constants and Variables}

Although it is possible to use Racket's @racket[define] and friends as normal in 6502 blocks, it can get quite verbose.  A shorthand for @racket[define] is provided in the format of @tt{ name = value }.

@#reader scribble/comment-reader
(racketblock
{
    a = $42
    b = %0000_1111
    c = (list 1 $2 %11)
code:blank
} 
)

These can appear in any 6502 block.  It can be quite useful to have a large block near the top of the file that holds global constants and variables.


@subsection{Memory Alignment}

Asi64 provides two commands for controlling the current assembly location.  The first is an absolute address in the format @tt{@literal|{*= $2000 }|}.  The second aligns the assembler to the nearest address divisible by the operand @tt{@literal|{/= $1000}| }.

@#reader scribble/comment-reader
(racketblock
{
   *= $2000   ;start assembling at $2000
   ; some code here ...   

   /= $1000   ;align to the nearest $1000 boundary

   ; some data table here ...            
code:blank
} 
)

Asi64 will warn you if it assembles over the top of a memory location it has previously written to.

@section[#:tag "labels"]{Labels}

@subsection{General}

Labels in Asi64 must always start with @tt{:}

When using a label as a target, omit the leading @tt{:} and suffix with @tt{:} @tt{@literal|{+}|} or @tt{@literal|{-}|}.  The latter two will look ahead or behind the current memory location and use the first label that is found with the given name.  This allows you to have many labels with the same name.

The assembler will uniquely number any labels with the same name before passing them to the emulator, to ensure they always appear in the disassembly.

If instead you suffix the label target with @tt{:}, the assembler will warn if more than one label exists with that name.

Labels can appear in most places, including before raw data and operands.

The following example shows various absolute labels including an operand label that enables simple self-modifying code

@#reader scribble/comment-reader
(racketblock
{
:a  lda :b \@0
    sta $d021
    inc b:
    jmp a:
code:blank
} 
)

The following example shows multiple labels with the same name, and using relative labels with @tt{@literal|{+}|} and @tt{@literal|{-}|}

@#reader scribble/comment-reader
(racketblock
 {
       ;some zero-page address
       state = $42 
:loop
       ; wait for the raster to hit the bottom of the screen
       lda $d012
       cmp \@$ff
       bne loop-
       ldx state
       bne next+  ;not zero
       ; code here for 0...
       jmp loop-
:next  cpx \@1
       bne next+  ; not 1
       ; code here for 1 ...
       jmp loop-
:next  cpx \@2     
       ; .. and so on

code:blank
} 
)

@subsection{Loading Label Locations}

A common assembler feature is to load a label location as an immediate value, for setting interrupt routines, self modifiyng code and the like. You can extract the low and high bytes from a label using @tt{@literal|{<}|} and @tt{@literal|{>}|} as you can in most assemblers.

@#reader scribble/comment-reader
(racketblock
{
    lda \@<int:    ; low part of address of interrupt handler code
    ldx \@>int:    ; high part of address of interrupt handler code   
    sta $0314    ; store in interrupt vector
    stx $0315

:int ; interrupt handler here
code:blank    
})

Finally, a macro @tt{label-loc} will return the 16bit value of the label directly (following the suffix rules) so that you can use it as part of an expression. For example

@#reader scribble/comment-reader
(racketblock
{
:a  lda \@0
    sta $d021
    inc (+ (label-loc a-) 1) ;self modifing code
    jmp a-
code:blank
}
)
This currently only works if the label was defined before the macro use in the source.

The above example is for illustration only, it could be written easier using an operand label like so, as per the example in the previous section.

@#reader scribble/comment-reader
(racketblock
{
:a  lda :b \@0
    sta $d021
    inc b:
    jmp a:
code:blank
}
)

@section[#:tag "racket integration"]{Racket Integration}

Even inside { } blocks, you still have all of Racket. As long as the code ends up being something the assembler expects, you can write whatever you like.  You can also write functions that return ('inline') 6502 blocks in the manner you would expect.
@#reader scribble/comment-reader
(racketblock

(define (mov src dst)	{   
	lda \@src
	sta dst
code:blank
})

(C64 {
	*=$1000
	(define x $42)	
	lda \@(+ x 1)
	sta $d021
        (mov (+ x x) (- $d021 1))
code:blank
})
)

6502 blocks can also be nested inside of each other, enabling you to mix arbitary Racket and 6502 forms wherever you like in the manner you would expect.

@#reader scribble/comment-reader
(racketblock
{
	*=$1000
	lda \@$20
	;unroll some code to clear a bit of the video memory
	(for ([i (in-range 0 10)])
	  (let ([address (+ $0400 i)])
	    {sta address}))
code:blank
})


@section{Pseudo-Ops}

In the previous example, the function @tt{mov} determines the immediate addressing mode itself, since ultimately it is part of the opcode rather than the operand. This greatly reduces the reusablity of the code since you'd have to have another @tt{mov} to move from a location in memory. The macro @tt{(define-op ex)} will re-write your function so that the addressing mode is determined at the call site.

@#reader scribble/comment-reader
(racketblock

(define-op (mov src dst) {
    lda src
    sta dst
code:blank
})

(C64 {
    *=$1000
    (mov \@42 $20)
    (mov $2000 $20)
    (mov $20 $2000)	
code:blank
})
)

The macro also introduces a few values for you to use that give metadata about the parameters. Currently you can use
@itemlist[
 @item{param-name-16bit? True if the operand is a 16 bit immediate value or 16-bit memory address}
@item{param-name-immediate? True if the parameter is immediate.}
]

These let you do some cool things such as writing general operations that are intelligent about their operands. Example:
@#reader scribble/comment-reader
(racketblock
{ 
;;; adds to a 16 bit number, little-endian fashion.
;;; detects 8 bit immediate numbers and optimises
;;; as appropriate. for absolute, assumes 16bit to 16bit
(define-op (add16 source dest )  
   (if source-16bit? {
     clc
     ;16 bit 
     lda dest
     adc (lo-byte source)
     sta dest
     lda (hi-byte source)
     adc (+ dest 1)
     sta (+ dest 1)
     }
    {clc
     lda dest
     adc (lo-byte source)
     sta dest
     bcc (+ (here)  3)
     inc (+ dest 1)
     }))
code:blank
})

This example could be better, it doesn't deal with 8 bit to 16 bit memory locations.

You might notice some other handy functions being used here - @tt{(lo-byte)} @tt{(high-byte)} and @tt{(here)}. The latter will yield the current instruction location and is handy for infinte loops, skipping instructions and self-modifing code, without having to use labels.

Pseudo-ops can sometimes be nested in each other. (working on this!)

@section{Data}

To insert blocks of arbitrary binary data, use the @tt{(data ...)} macro.   It accepts any number of arguments which it will write directly as bytes.  If an argument is a Racket @racket[list], it will be recusrively unwrapped and its ultimate contents written to memory.

@racketblock[
{
  (define (deg->rad rad)
    (* (/ pi 180) rad))

:sine
  (data
    (for/list ([x (in-range 0 365 5)])
      (bitwise-and (exact-round (* (sin (deg->rad x)) 100)) #xFF)))

code:blank
}]

@section{Code Diagnostics}

Asi64 knows about the 6502 instruction set and is able to tell you useful information about your code, such as how many cycles each instruction takes, which processor flags they might affect, and how much space the block occupies.

Simply wrap any code you are interestied in seeing metrics about between @tt{?=} and @tt{=?}. When you assemble, everything will proceed as normal, but you will also see the information about your code blocks in the output window.

@#reader scribble/comment-reader
(racketblock
{ 
?= ;begin diagnostics
:sprite-y-char-top
     lda $d001
     sec
     sbc \@$32       
     lsr		
     lsr
     lsr
     tax
     lda screen-rows-lo: x
     sta screen-lo
     lda screen-rows-hi: x
     sta screen-hi
     rts
=? ;end
code:blank
})
 
this produces :


@codeblock{
diagnostics started at $30ec
opcode a-mode cycles flags
lda    abs    4      (Z N)
sec    none   2      (C)
sbc    i      2      (C Z V N)
lsr    none   2      (C Z N)
lsr    none   2      (C Z N)
lsr    none   2      (C Z N)
tax    none   2      (Z N)
lda    absx   4/5    (Z N)
sta    zp     3      ()
lda    absx   4/5    (Z N)
sta    zp     3      ()
rts    none   6      ()
diagnostics finished at $3101
total code size $15 (21)  min/max cycles (36/38)
}

Note that since you can put numbers and data anywhere, asi64 can only show you information about code assembled directly with the assembler syntax. If you put a @tt{(data ...)} block in the middle of the code, the diagnostics will simply ignore it, even if the bytes equate to valid opcode(s).

@section{Metaprogramming Helpers}

When you want to write code that writes other code, asi64 has a macro that helps you load the correct value for the combination of opcode and addressing mode you require. For example :

@#reader scribble/comment-reader
(racketblock
{
   lda \@(infer sta £ $ff y)
code:blank
}
)

This code will load the accumulator with the immediate value of @tt{$91}, the byte that represents the @tt{sta} opcode in its indirect, y-offset addressing mode.

Of course, the actual address @tt{$ff} here makes no difference, it is simply used to infer the addressing mode. If you had wrote @tt{$ffff} it would have produced an error since the sta indirect addressing modes do not work with 16 bit addresses.

Unlike the rest of the assembler, you cannot use labels, expressions and other features within the infer macro, since it would make no sense to do so.

@section[#:tag "emulator configuration"]{Emulator Configuration}

Emulator conifguration is achieved by setting fields of a struct that is exposed directly to the programmer.

@#reader scribble/comment-reader
(racketblock
(struct emulator
  (path           ; location of the emu executable
   program        ; assembled file target path
   breakpoints?   ; pass along breakpoints
   labels?        ; pass along labels
   execute?)      ; execute the emu post assembly
  #:mutable)
)

An instance of this struct @tt{emu} is exposed and the programmer can set the fields in the typical Racket style.

@#reader scribble/comment-reader
(racketblock
  (set-emulator-execute?! emu #f) ; stop the emulator executing 
)

It can be useful to prevent the emulator executing after every assemble when working on long pieces of code or using the diagnostics facility to cycle count.

Note that the @tt{program} field is not strictly associated with the emulator.  It is included here since it seems unlikely you would want to use asi64 without an emulator.  In the future, if asi64 is expanded to support other emulators, machines and same-family chips (NES, GameBoy, SNES ... ) this minimal design will likely change to something slightly more abstract.

An additional convenience function is provided that sets all the fields in one call.

@#reader scribble/comment-reader
(racketblock
(define (configure-emu emu-path program-path execute-emu? enable-breakpoints?)
  (set-emulator-program! emu program-path)
  (set-emulator-execute?! emu execute-emu?)
  (set-emulator-breakpoints?! emu enable-breakpoints?)
  (set-emulator-path! emu emu-path))
  (set-emulator-execute? emu #f)
)


@section{Interesting Examples}

Coming Soon!
