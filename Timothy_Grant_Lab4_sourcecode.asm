;***********************************************************
;*
;*	Enter name of file here
;*
;*	Enter the description of the program here
;*
;*	This is the skeleton file for Lab 4 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Timothy Lee Grant
;*	   Date: Oct 23, 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register is
								; required for LCD Driver

.def counter = r23				; register for counting

.equ clearBit = 7				; 7th bit pressed to clear screen
.equ upward = 1					; 1st bit pressed for normal display
.equ downward = 2				; 2nd bit pressed for upside down display

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp INIT				; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine
		; Initialize Stack Pointer
		ldi mpr, high(RAMEND)	; place high value of Max RAM mem address into mpr
		out SPH, mpr			; initilize the stack high byte 

		ldi mpr, low(RAMEND)	; place the low value of Max RAM mem address into mpr
		out SPL, mpr			; initilize the stack pointer low byte

		; Initialize LCD Display
		call LCDInit
		; NOTE that there is no RET or RJMP from INIT, this
		; is because the next instruction executed is the
		; first instruction of the main program

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program
		; Main function design is up to you. Below is an example to brainstorm.
		
		; Move strings from Program Memory to Data Memory

		;if first pin pressed, display rightside up
		in mpr, PIND					;load pinD input to mpr
		ror mpr							;rotate right once to get first (0th bit) into carry bit
		brcs skip1						;skip entire "rightSide Up function if not pressed

		
		call LCDClr						;clear previous screen settings

		;right side up function

		;get the Y and Z register to point to the location in program memory
		ldi XH, $01						;following two commands store location 0x0100 into X pointer (this is the location of top line)
		ldi XL, $00
		ldi ZL, low(STRING_BEG<<1)		;Z points to the BEG str (the first one)
		ldi ZH, high(STRING_BEG<<1)

		;load first string into data memory the first string
		ldi counter, $0E				; loop to run 14 times (because 14 characters in first string)
		loadFirstNormal:
			lpm r16, Z+					;first string char into r16 (to later be placed into data memory)
			st X+, r16					;place into data memory 
			dec counter					;decrease counter
			brne loadFirstNormal		; keep looping until reach count == 0

		;load second string into data memory the second string
		ldi XH, $01						;following two commands store location 0x0110 into X pointer
		ldi XL, $10
		ldi ZL, low(STRING_END<<1)		;Z points to the END str (the second one)
		ldi ZH, high(STRING_END<<1)
		ldi counter, $B					;loop 11 times (because 11 characters in second string)
		loadSecondNormal:
			lpm r16, Z+					;second string char into r16(to later be placed into data memory)
			st X+, r16					;place into data memory
			dec counter					;decrease counter
			brne loadSecondNormal		;keep looping until count == 0

		skip1:

		;if second button is pressed, write upside down
		in mpr, PIND					;store pinD input into mpr
		ror mpr							;rotate right twice (to get the second bit into the carry flag)
		ror mpr
		brcs skip2						;if carry flag is not pressed (set) skip over upside down write function

		call LCDClr						;clear previous screen settings

		;get the Y and Z register to point to the location in program memory
		ldi XH, $01						;following two commands store location 0x0110 into X pointer (location of the bottom line)
		ldi XL, $10
		ldi ZL, low(STRING_BEG<<1)		;Z points to the BEG string in program memory (the first string)
		ldi ZH, high(STRING_BEG<<1)

		;load into data memory the first string
		ldi counter, $0E				;loop to run 14 times (because 14 characters in first string)
		loadSecondAbnormal:
			lpm r16, Z+					;place the contents that Z points to into r16 (to be moved into data memory later), then increase Z to point to the next character in the string
			st X+, r16					;place into data memory
			dec counter					;decrease counter
			brne loadSecondAbnormal		;keep looping until reach count == 0

		;load into data memory the second string
		ldi XH, $01						;following two commands store location 0x0100 into X pointer
		ldi XL, $00
		ldi ZL, low(STRING_END<<1)		;Z points to the END string in program memory (the second string)
		ldi ZH, high(STRING_END<<1)
		ldi counter, $B					;loop 11 times (because 11 characters in second string)
		loadFirstAbnormal:
			lpm r16, Z+					;place contents that Z points to into r16 (to later be moved into data memory), then increase Z to point to the next character in the string
			st X+, r16					;place into data memory
			dec counter					;decrease counter
			brne loadFirstAbnormal		;keep looping until reach count == 0

		skip2:

		;check if clear signal (bit 7) is pressed
		in mpr, PIND					;get input and store into mpr
		ror mpr							;rotate right 8 times to get final bit into carry flag
		ror mpr
		ror mpr
		ror mpr
		ror mpr
		ror mpr
		ror mpr
		ror mpr
		brcs skip						;if 8th pin in pressed (active low), then proceed. If carry is high, skip the clear LCD function
		call LCDClr
		skip:

		; Display the strings on the LCD Display
		call LCDWrLn1
		call LCDWrLn2

		
		 

		rjmp	MAIN			; jump back to main and create an infinite
								; while loop.  Generally, every main program is an
								; infinite while loop, never let the main program
								; just run off

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variables by pushing them to the stack

		; Execute the function here

		; Restore variables by popping them from the stack,
		; in reverse order

		ret						; End a function with RET

;***********************************************************
;*	Stored Program Data
;***********************************************************

;----------------------------------------------------------- 
; An example of storing a string. Note the labels before and
; after the .DB directive; these can help to access the data
;-----------------------------------------------------------
;;;;; I think these values are stored in the program memory, I will need to access them through the lpm command
STRING_BEG:
.DB		"Timothy Grant "		; Declaring data in ProgMemory
STRING_END:
.DB "Hello World "

;***********************************************************
;*	Additional Program Includes
;***********************************************************
.include "LCDDriver.asm"		; Include the LCD Driver
