;***********************************************************
;*
;*	Enter Name of file here
;*
;*	Enter the description of the program here
;*
;*	This is the skeleton file for Lab 7 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Timothy Lee Grant
;*	   Date: 12 Nov, 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register
.def	mpr2 = r17				; second MPR
.def	waitcnt = r20
.def	ilcnt = r21
.def	olcnt = r22
.def	counter = r23			;counter for brightness level

.equ	EngEnR = 4				; right Engine Enable Bit
.equ	EngEnL = 7				; left Engine Enable Bit
.equ	EngDirR = 5				; right Engine Direction Bit
.equ	EngDirL = 6				; left Engine Direction Bit
.equ	WTime = 20

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt
		
.org	$0002
		rcall	BRIGHTER
		reti

.org	$0004
		rcall	DIMMER
		reti

.org	$0006
		rcall	TurnOn
		reti

.org	$0008
		rcall	TurnOff
		reti

.org	$0046					; end of interrupt vectors

;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:
		; Initialize the Stack Pointer
		ldi mpr, high(RAMEND)
		out SPH, mpr
		ldi mpr, low(RAMEND)
		out SPL, mpr
		; Configure I/O ports
			
			;PortB is output
			ldi mpr, $FF			; configure (PortB) as output
			out DDRB, mpr
			ldi mpr, $00			; set on PORTB to be all 0
			out PORTB, mpr
			;PortD is input
			ldi mpr, $00			;configure (PortD) as input
			out DDRD, mpr
			ldi mpr, $FF
			out PORTD, mpr

		; Configure External Interrupts, if needed
		ldi mpr, 0b10101010
		sts EICRA, mpr

		ldi mpr, 0b00001111
		out EIMSK, mpr
			

		; Configure 8-bit Timer/Counters

		; 0b01001000 (wave generation mode set to fast PWM)
		; 0b00110000 (inverting compare mode)
		; 0b00000001 (no prescaling mode)
		ldi mpr, 0b01111001		;OR the modes above
		out TCCR0, mpr
		out TCCR2, mpr

		; Set TekBot to Move Forward (1<<EngDirR|1<<EngDirL)
		ldi mpr, (1<<EngDirR|1<<EngDirL)
		out PORTB, mpr

		; Set initial speed, display on Port B pins 3:0
		ldi mpr, $FF			; setting LED brightness to be off
		out OCR0, mpr
		out OCR2, mpr

		ldi counter, $00

		; Enable global interrupts (if any are used)
		sei

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		; poll Port D pushbuttons (if needed)

								; if pressed, adjust speed
								; also, adjust speed indication

		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************


;-----------------------------------------------------------
; Func:	Template function header
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
BRIGHTER:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push mpr
		push r17
		; Execute the function here

		ldi mpr, $11		; load value to step by
		in r17, OCR0		; place current brightness level in r17
		sub r17, mpr		; subtract step from current brightness (result = brighter)
		brcs CARRY1			; if carry set, value below MIN val (load MIN val instead)
		out OCR0, r17		; load value into OCR0
		out OCR2, r17		; load value into OCR0
		inc counter
		ori counter, (1<<EngDirR|1<<EngDirL)
		out PORTB, counter
		rjmp PASS1

		CARRY1:
		ldi mpr, $00		; if carry occured during step, set min val to OCR0/OCR2
		out OCR0, mpr
		out OCR2, mpr
		ldi counter, $FF
		out PORTB, counter


		PASS1:

		ldi waitcnt, WTime	; small wait to help debouncing
		rcall WaitFunction

		ldi mpr, 0b00001111	; reset interrupts for INT0:3
		out EIFR, mpr
		; Restore any saved variables by popping from stack
		pop r17
		pop mpr

		ret						; End a function with RET


;-----------------------------------------------------------
; Func:	Template function header
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
DIMMER:	; Begin a function with a label

		; If needed, save variables by pushing to the stack
		push mpr
		push r17
		; Execute the function here
		
		ldi mpr, $11		; load value to step by
		in r17, OCR0		; place current brightness level in r17
		add r17, mpr		; add step from current brightness (result = dimmer)
		brcs CARRY2			; if carry set, value above MAX val (load MAX val instead)
		out OCR0, r17		; load value into OCR0
		out OCR2, r17		; load value into OCR0
		dec counter
		ori counter, (1<<EngDirR|1<<EngDirL)
		out PORTB, counter
		rjmp PASS2

		CARRY2:
		ldi mpr, $FF		; if carry occured during step, set max val to OCR0/OCR2
		out OCR0, mpr
		out OCR2, mpr
		ldi counter, $00
		ori counter, (1<<EngDirR|1<<EngDirL)
		out PORTB, counter


		PASS2:

		ldi waitcnt, WTime	; small wait to help debouncing
		rcall WaitFunction

		ldi mpr, 0b00001111		; reset interrupts for INT0:3
		out EIFR, mpr

		; Restore any saved variables by popping from stack
		pop r17
		pop mpr

		ret						; End a function with RET



TurnOn:
		push mpr

		ldi mpr, $00							; load min value (min val = max bright)
		out OCR0, mpr							; place value in OCR0/OCR2
		out OCR2, mpr
		ldi counter, $FF						; set counter to max
		ori counter, (1<<EngDirR|1<<EngDirL)	; or counter with Forward motion
		out PORTB, counter						; send data to PORTB (to LEDS)

		ldi waitcnt, WTime						; wait time for debouncing
		rcall WaitFunction

		ldi mpr, 0b00001111						; reset interrupts for INT0:3
		out EIFR, mpr

		pop mpr
		ret

TurnOff:
		push mpr

		ldi mpr, $FF							; load max value (max val = min bright)
		out OCR0, mpr							; place value in OCR0/OCR2
		out OCR2, mpr
		ldi counter, $00						; set counter to min
		ori counter, (1<<EngDirR|1<<EngDirL)	; or counter with Forward motion
		out PORTB, counter						; send data to PORTB (to LEDS)

		ldi waitcnt, WTime						; wait time for debouncing
		rcall WaitFunction

		ldi mpr, 0b00001111						; reset interrupts for INT0:3
		out EIFR, mpr

		pop mpr
		ret
		ret




WaitFunction:
		push waitcnt
		push ilcnt
		push olcnt

		Loop:
			ldi olcnt, 224
		OLoop:
			ldi ilcnt, 237
		ILoop:
			dec ilcnt
			brne ILoop
			dec olcnt
			brne OLoop
			dec waitcnt
			brne Loop

		pop olcnt
		pop ilcnt
		pop waitcnt
		ret
;-----------------------------------------------------------
; Func:	Template function header
; Desc:	Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:	; Begin a function with a label

		; If needed, save variables by pushing to the stack

		; Execute the function here
		
		; Restore any saved variables by popping from stack

		ret						; End a function with RET



;***********************************************************
;*	Stored Program Data
;***********************************************************
		; Enter any stored data you might need here

;***********************************************************
;*	Additional Program Includes
;***********************************************************
		; There are no additional file includes for this program