# asi64

asi64 is a full 6502 assembler, primarily aimed at programming the Commodore 64.

Traditional assemblers provide various scripting and macro facilities to ease the tedium of writing assembly code and generating data.  asi64 takes a very different approach, and instead extends the [Racket language](https://racket-lang.org/) to become an assembler.  This means you get the entirety of the racket language at your disposal to help write your assembly.  Racket's extremely powerful macro system along with functional, object and imperative paradigms, and a huge, mature standard library are all accessible.

Various posts utilising the assembler can be found at my blog [here](http://pinksquirrellabs.com/tags/6502.html)

## Disclaimer
This is a tool written primarily for my own enjoyment and education (learning 6502, the C64 and Racket).  At the moment, it is not very user-friendly. Consider this an alpha and playground that is liable to change a great deal at any moment.  Having said that, if you use and like this, or do something cool with it, [please let me know!](https://twitter.com/pezi_pink)

## Getting Started
You can find asi64 on Racket's package manager. `raco pkg install asi64` should get you setup and ready to go.
Create a racket file somewhere for your program.  Here is a minimal example.
 
 ```asm
#lang asi64

(set-emulator-program! emu "c64.prg")
(set-emulator-execute?! emu #t)
(if (eq? (system-type 'os) 'windows)
   (set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")
   (set-emulator-path! emu "/snap/bin/vice-jz.x64"))

(C64 {
       *= $1000   ;start at $1000
:loop inc $d021  ;flash border
       jmp loop-  ;loop forever
})
 ```
 asi64 supports various emulator features.  Currently, it is targetted at [Vice](http://vice-emu.sourceforge.net/). In the preceding program, we tell the assembler to create a file called c64.prg, and pass along the location of the C64 emulator.

 The 6502 program itself is expected to be in a single `C64` form.  Anything between `{ ... } ` will be seen as 6502 assembler - more on the full syntax and features in just a moment.

 If you compile and run this program, it should assemble and pass the resulting binary file to the emulator for you to enjoy.

## Assembler Syntax
I tried to keep as close to normal 6502 asm as possible, however since this extends racket, I had to make some compromises.

* `*=` instructs the assembler to move to and assemble from the memory location provided
* You can use $ and % for hex and binary literals.  These work anywhere, not just inside {  } blocks.
* Binary literals can contain any amount of `_` characters.
* `@` is used for immediate addressing mode. The traditional `lda #42` is written as `lda @42`
* Indexed addressing modes are the same, but without a comma.  `sta $0400,x` becomes `sta $0400 x`
* Indirect addressing modes I had to butcher a bit, since the traditional parens would have messed everything up.  Currently, it is denoted by the `Â£` character, as it is the only one I could find that isn't used for something in racket already.  Therefore,  `sta ($0400),y` becomes `sta Â£ $0400 y` and `jmp ($4000)` becomes `jmp Â£ $4000`
* `/=` aligns the assembler to the next memory location evenly divisble by the value supplied
* `?=` and `=?` switch on and off compile time diagnostics.  See the Diagnostics section below for more information.


### Labels
In asi64, a label name must start with `:`.  Labels can appear on their own line, before an opcode and/or before an operand.  The latter is especially useful for self modifiying code.

When using a label as a target, the suffix determines how it is resolved.  `+` and `-` will jump to the closest label with that name in front or behind the current location in memory.  This allows you to have many labels with the same name.  Otherwise, you must specify `:` as a suffix.

### More Labels

A common assembler feature is to load a label location as an immediate value, for setting interrupt routines, self modifiyng code and the like.  You can extract the low and high bytes from a label using < and > as you can in most assemblers.

```asm
    lda @<int:    ; low part of address of interrupt handler code
    ldx @>int:    ; high part of address of interrupt handler code   
    sta $0314    ; store in interrupt vector
    stx $0315

:int ; interrupt handler here    
```
Finally, a macro `label-loc` will return the 16bit value of the label directly (following the suffix rules) so that you can use it as part of an expression.  For example

```asm
:a  lda @0
    sta $d021
    inc (+ (label-loc a-) 1) ;self modifing code
    jmp a-
```

Note this currently only works if the label was defined before the macro use in the source. (TODO)

The above example is for illustration only, it could be written easier using an operand label like so.

```asm
:a  lda :b @0
    sta $d021
    inc b:
    jmp a:
```

### Expressions
Even inside `{ }` blocks, you still have all of racket.  As long as the code ends up being something the assembler expects, you can write whatever you like.

```asm
(C64 {
	*=$1000
	(define x $42)	
	lda @x
	sta $d021
	lda @(+ x x)
	sta (- $d021 1)
})
```

6502 blocks can also be nested inside of each other, enabling you to mix arbitary racket and 6502 forms wherever you like in the manner you would expect.

```asm
(C64 {
	*=$1000
	lda @$20
	;unroll some code to clear a bit of the video memory
	(for ([i (in-range 0 10)])
	  (let ([address (+ $0400 i)])
	    {sta address}))
})
```

### Constants
It is inconvenient to write `(define x $42)`  everywhere you need a constant.  A shorthand is provided that works within any 6502 block

```asm
{
        a = $42
        b = %0000_1111
        c = (list 1 $2 %11) 
}
```

Whilst this works within any nested 6502 block, it can be quite useful to define a section of these near the top of your file, outside of the `C64` form.
 

### Functions
You can define and call racket functions that yield assembly code.

```asm
(define (mov src dst)	{   
	lda @src
	sta dst
})

(C64 {
	*=$1000
	(mov 42 $d021)
})
```

### Pseudo-ops (Experimental!)

In the previous example, the function `mov` determines the immediate addressing mode itself, since ultimately it is part of the opcode rather than the operand.  This greatly reduces the reusablity of the code since you'd have to have another `mov` to move from a location in memory.  The macro `(define-op ex)` will re-write your function so that the addressing mode is determined at the call site.

```asm
(define-op (mov src dst)) {
    lda src
	sta dst
})

(C64 {
	*=$1000
	(mov @42 $20)
	(mov $2000 $20)
	(mov $20 $2000)	
})
```
The macro also introduces a few values for you to use that give metadata about the parameters.  Currently you can use

* param-name-16bit?  This will be true if the operand is a 16 bit immediate value or 16-bit memory address
* param-name-immediate?  True if the parameter is immediate.

These let you do some cool things such as writing general operations that are intelligent about their operands.  Example:

```asm
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
} ))
```
(this example could be better, it doesn't deal with 8 bit to 16 bit memory locations)

You might notice some other handy functions being used here.  `(lo-byte)` `(high-byte)` and `(here)`.  The latter will yield the current instruction location and is handy for infinte loops, skipping instructions and self-modifing code, without having to use labels.

Pseudo-ops can sometimes be nested in each other. (working on this!)

### Emulator support

Currently only Vice is supported.  The labels you define are passed to the emulator so you will see them in the monitor's disassembly.  There is also a special `break` instruction which will enable that location as a breakpoint in the emulator, greatly simplifying your debugging experience.

```asm

(set-emulator-program! emu "C64.prg")
(set-emulator-execute?! emu #t)
(set-emulator-breakpoints?! emu #t)
(set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")

(C64 {
	*=$1000
	inc $d021
	break
	jmp (- (here) 3)
})

```

### Data

Commonly you need to generate tables of data.  For this, there is a (very simple indeed) `data` macro, which will let you write some expression to generate a bunch of numbers at the current location, which you can of course label.

```asm
(C64{
	*=$1000
	lda mystuff+
	sta $d021
	ldx @1
	lda mystuff+ x
	sta $d020

	/=$1000  ; align to next $1000 location, eg $2000
:mystuff   
	(data %10000000 $FF (for ([i (in-range 1 10)]) i))
})
```

### Diagnostics

Asi64 knows about the 6502 instruction set and is able to tell you useful information about your code, such as how many cycles each instruction takes, which processor flags they might affect, and how much space the block occupies.

Simply wrap any code you are interestied in seeing metrics about between `?=` and `=?`.  When you assemble, everything will proceed as normal, but you will also see the information about your code blocks in the output window.

```asm
?= ;begin diagnostics
:sprite-y-char-top
     lda $d001
     sec
     sbc @$32       
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
```

this produces :

```
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
total code size $15 (21).  min/max cycles (36/38)
```

Note that since you can put numbers and data anywhere, asi64 can only show you information about code assembled directly with the assembler syntax.  If you put a `(data ...)` block in the middle of the code, the diagnostics will simply ignore it, even if the bytes equate to valid opcode(s).


### Metaprogramming helpers

When you want to write code that writes other code, asi64 has a macro that helps you load the correct value for the combination of opcode and addressing mode you require. For example :

```racket
{
   lda @(infer sta £ $ff y)
}

```

This code will load the accumulator with the immediate value of `$91`, the byte that represents the `sta` opcode in its indirect, y-offset addressing mode.  

Of course, the actual address `$ff` here makes no difference, it is simply used to infer the addressing mode.  If you had wrote `$ffff` it would have produced an error since the `sta` indirect addressing modes do not work with 16 bit addresses. 

Unlike the rest of the assembler, you cannot use labels, expressions and other features within the `infer` macro, since it would make no sense to do so.


### Programming the assembler internals

Full access to the assembler itself is exposed allowing you to insepct and modify it at will

TODO

### Helper Library
TODO:

There will be helper library defining a bunch of common C64 routines and constants.
