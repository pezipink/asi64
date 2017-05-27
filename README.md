# Asi64

Asi64 is a full 6502 assembler, primarily aimed at programming the Commodore 64.

Traditional assemblers provide various scripting and macro facilities to ease the tedium of writing assembly code and generating data.  Asi64 takes a very differnt approach, and instead extends the Racket language to become an assembler.  This means you get the entire of the racket language at your disposal to help write your assembly.  Racket's extremely powerful macro system along with functional, object and imperative paradigms, and a huge, mature standard library are all accessible.

## Disclaimer
This is a tool written primarily for my own emjoyment and eduation.  At the moment, it is not very user-friendly. Consider this a pre-alpha and playground that is liable to change a great deal at any moment.  Having said that, if you use and like this, or do something cool with it, [please let me know!](https://twitter.com/pezi_pink)

## Getting Started
You can find Asi64 on Racket's package manager. `raco install Asi64` should get you setup and ready to go.
Create a racket file somewhere for your program.  Here is a minimal example.
 
 ```racket
#lang reader (lib "Asi64")
(require (lib "Asi64/expander.rkt"))

(set-emulator-program! emu "c64.prg")
(set-emulator-execute?! emu #t)
(set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")

(C64 {
      *= $1000   ;start at $1000
:loop inc $d021  ;flash border
      jmp loop-  ;loop forever
})
 ```

 Asi64 is not a full `#lang` just yet.  Since it extends racket, the first line redirects the normal Racket reader to use Asi64's instead.  The actual assembler and macro expander layer is also required if you want anything interesting to happen.

 Asi64 supports various emulator features.  Currently, it is targetted at [WinVice](http://vice-emu.sourceforge.net/) (I guess the mac version should work as well with a small tweak or two, PRs welcome!). In the preceding program, we tell the assembler to create a file called c64.prg, and pass along the location of the C64 emulator.

 The 6502 program itself is expected to be in a single `C64` form.  Anything between `{ ... } ` will be seen as 6502 assembler - more on the full syntax and features in just a moment.

 If you compile and run this program, it should assemble and pass the resulting binary file to the emulator for you to enjoy.

## Assembler Syntax
I tried to keep as close to normal 6502 asm as possible, however since this extends racket, I had to make some compromises.

* You can use $ and % for hex and binary literals.  These work anywhere, not just inside {  } blocks.
* `@` is used for immediate addressing mode. The traditional `lda #42` is written as `lda @42`
* Indexed addressing modes are the same, but withouht a comma.  `sta $0400,x` becomes `sta $0400 x`
* Indirect addressing modes I had to butcher a bit, since the traditional parens would have messed everything up.  Currently, it is denoted by the `£` character, as it is the only one I could find that isn't used for something in racket already.  Therefore,  `sta ($0400),y` becomes `sta £ $0400 y`


### Labels
In Asi64, a label name must start with `:`.  Labels can currently appear on their own line, or before an opcode.

When using a label as a target, the suffix determines how it is resolved.  `+` and `-` will jump to the closest label with that name in front or behind the current location in memory.  This allows you to have many labels with the same name.  Otherwise, you must specify `:` as a suffix.

### More Labels

A common assembler feature is to load a label location as an immediate value, for setting interrupt routines, self modifiyng code and the like.  You can extract the low and high bytes from a label using < and > as you can in most assemblers.

```racket
    lda @<int:    ; low part of address of interrupt handler code
    ldx @>int:    ; high part of address of interrupt handler code   
    sta $0314    ; store in interrupt vector
    stx $0315

:int ; interrupt handler here    
```
Finally, a macro `label-loc` will return the 16bit value of the label directly (following the suffix rules) so that you can use it as part of an expression.  For example

```racket
:a  lda @0
    sta $d021
    inc (+ (label-loc a-) 1) ;self modifing code
    jmp a-
```

### Expressions
Even inside `{ }` blocks, you still have all of racket.  As long as the code ends up being something the assembler expects, you can write whatever you like.

```racket
(C64 {
	*=$1000
	(define x $42)	
	lda @x
	sta $d021
	lda @(+ x x)
	sta (- $d021 1)
})
```

6502 blocks can also be nested inside of each other, enabling you mix arbitary racket and 6502 forms wherever you like in the manner you would expect.

```racket
(C64 {
	*=$1000
	lda @$20
	;unroll some code to clear a bit of the video memory
	(for ([i (in-range 0 10)])
		(let ([address (+ $0400 i)])
			{sta address}))
})
```

### Functions
You can define and call racket functions that yield assembly code.

```racket
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

In the previous example, the function `mov` determines the immediate addressing mode itself, since ultimately it is part of the opcode rather than the operand.  This greatly reduces the reusablity of the code since you'd have to have another `move` to move from a location in memory.  The macro `(define-op ex)` will re-write your function so that the addressing mode is determined at the call site.

```racket
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

* param-name-16bit?  This will be true if the operand is a 16 bit immediate value or memory address
* param-name-immedaite?  True if the parameter is immediate.

These let you do some cool things such as writing general operations that are intelligent about their operands.  Example:

```racket
(define-op (mov src dst)) {
    lda src
	lda dst
})

```racket
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

Pseudo-ops can also be nested in one another (this seems to mostly work, no gurantees!)

### Emulator support

Currently only Vice is supported.  The labels you define are passed to the emulator so you will see them in the monitor's disassembly.  There is also a special `break` instruction which will enable that location as a breakpoint in the emulator, so greatly simplify your debugging experience.

```racket

(set-emulator-program! emu "C64.prg")
(set-emulator-execute?! emu true)
(set-emulator-breakpoints?! emu true)
(set-emulator-path! emu "C:\\Program Files\\WinVICE-3.0-x64\\x64.exe")

(C64 {
	*=$1000
	inc $d021
	break
	jmp (- (here) 3)
})

```

### DATA

Commonly you need to generate tables of data.  For this, there is a (very simple indeed) `data` macro, which will let you write some expression to generate a bunch of numbers at the current location, which you can of course label.

```racket
(C64{
	*=$1000
	lda mystuff+
	sta $d021
	ldx @1
	lda mystuff+ x
	sta $d020

:mystuff   
	(data %10000000 $FF (for ([i (in-range 1 10)]) i))
})


### Programming the assembler internals

Full access to the assembler itself is exposed allowing you to insepct and modify it at will

TODO