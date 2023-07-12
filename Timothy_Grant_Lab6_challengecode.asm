;***********************************************************
;*
;*	Lab 6
;*
;*	Working with inturputs
;*
;*	This is the skeleton file for Lab 6 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Timothy Lee Grant
;*	   Date: 11 Nov 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;************************************************************
;* Variable and Constant Declarations
;************************************************************
.def	mpr = r16				; Multi-Purpose Register
.def	waitcnt = r23				; Wait Loop Counter
.def	ilcnt = r24				; Inner Loop Counter
.def	olcnt = r25				; Outer Loop Counter
.def	RightCount = r10		; keep track of hit right count
.def	LeftCount = r11			; keep track of hit left count
.def	ConsecLeft = r12		; keep track of consequtive left hits
.def	consecRight	= r13		; keep track of consequitive right hits
.def	alternating = r14

.equ	WTime = 100				; Time to wait in wait loop

.equ	WskrR = 0				; Right Whisker Input Bit
.equ	WskrL = 1				; Left Whisker Input Bit
.equ	EngEnR = 4				; Right Engine Enable Bit
.equ	EngEnL = 7				; Left Engine Enable Bit
.equ	EngDirR = 5				; Right Engine Direction Bit
.equ	EngDirL = 6				; Left Engine Direction Bit

;/////////////////////////////////////////////////////////////
;These macros are the values to make the TekBot Move.
;/////////////////////////////////////////////////////////////

.equ	MovFwd = (1<<EngDirR|1<<EngDirL)	; Move Forward Command
.equ	MovBck = $00				; Move Backward Command
.equ	TurnR = (1<<EngDirL)			; Turn Right Command
.equ	TurnL = (1<<EngDirR)			; Turn Left Command
.equ	Halt = (1<<EngEnR|1<<EngEnL)		; Halt Command

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

		; Set up interrupt vectors for any interrupts being used
.org $0002						;$0002 is associated with INT0
	rcall HitRight
	reti
	

.org $0004		
	rcall HitLeft				; $0004 is associated with INT1
	reti

.org $0006
	rcall ClearRight
	reti

.org $0008
	rcall ClearLeft
	reti

		; This is just an example:
;.org	$002E					; Analog Comparator IV
;		rcall	HandleAC		; Call function to handle interrupt
;		reti					; Return from interrupt

.org	$0046					; End of Interrupt Vectors

.include "LCDDriver.asm"		; include the LCD driver 


;***********************************************************
;*	Program Initialization
;***********************************************************
INIT:							; The initialization routine

		; Initialize Stack Pointer
		ldi mpr, high(RAMEND)				; place high value of Max RAM mem address into mpr
		out SPH, mpr						; initilize the stack high byte
		ldi mpr, low(RAMEND)				; place the low byte of the mem address of Max RAM into mpr
		out SPL, mpr						; initilize the stack pointer low byte


		call LCDInit						; initiliaze the LCD screen
		call LCDClrLn1
		call LCDClrLn2	
		
		ldi mpr, $30						; load 0x30 into right and left count
		mov RightCount, mpr					; 0x30 corresponds to ascii 0
		mov LeftCount, mpr
		call LCDWrite						; print 0 to the screen for first and second line

		; Initialize Port B for output
		ldi mpr, $FF						; set PortB data direction register
		out DDRB, mpr						; for output
		ldi mpr, $00						; initialize port b data register
		out PORTB, mpr						; so all port b outputs are low

		; Initialize Port D for input
		ldi mpr, $00						; set port d data direction register
		out DDRD, mpr						; for input
		ldi mpr, $FF						; initialize port d data register
		out PORTD, mpr						; send command to motors

		; Consequtive hit
		ldi mpr, $00
		mov ConsecRight, mpr				; clear consecRight to 0
		mov ConsecLeft, mpr					; clear consecLeft to 0
		mov alternating, mpr				; clear alternating count to 0

		; Initialize external interrupts
			; Set the Interrupt Sense Control to falling edge 
			ldi mpr, 0b10101010				;sets INT0:INT3 to be falling edge
			sts EICRA, mpr

		; Configure the External Interrupt Mask
		ldi mpr, 0b00001111					; enable inturrupts 0:3
		out EIMSK, mpr

		; Turn on interrupts
		sei									; set global interrupt enable 
		

;***********************************************************
;*	Main Program
;***********************************************************
MAIN:							; The Main program

		; TODO: 
		; print current left and right hit value
		; keep going forward

		ldi mpr, MovFwd						; move forward
		out PORTB, mpr						; output to the PORTB the MovFwd command

		
		ldi		ZL, low(LCDLn1Addr)			; load the address of the first line into Z
		ldi		ZH, high(LCDLn1Addr)
		mov mpr, RightCount					; load the RightCount into mpr
		st Z, mpr							; place the RightCount at the mem loc that LCD prints first line

		;;;;;;
		;if(rightCount > 9)
		;	($0100) <- 1
		;	($0101) <- RightCount - 10
		;;;;;;
		cpi mpr, $3A						; check if number is greater than 9 (if greater need special attention)
		BRLO PASS1
			ldi mpr, $31					; first digit (tens place) should have a value of ascii 1
			st Z+, mpr						; place 1 in first byte of LCD
			mov mpr, RightCount
			SUBI mpr, $A					; subtract 10 from value to find out ones place value
			st Z, mpr						; put ones place value on the second character 
		PASS1:

		ldi		ZL, low(LCDLn2Addr)			; load location of second line into Z
		ldi		ZH, high(LCDLn2Addr)
		mov mpr, LeftCount					; put LeftCount into mpr
		st Z, mpr							; put the value of LeftCount into the location of second line

		cpi mpr, $3A						; check if number is greater than 9 (if greater need special attention)
		BRLO PASS2
			ldi mpr, $31					; put a ascii 1 in the tens place
			st Z+, mpr						; place 1 in first byte of LCD
			mov mpr, LeftCount
			SUBI mpr, $A					; subtract d10 from value to get ones place value
			st Z, mpr						; put the ones place in the second character location
		PASS2:

		call LCDWrite						; write to the screen

		rjmp	MAIN			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
;	You will probably want several functions, one to handle the 
;	left whisker interrupt, one to handle the right whisker 
;	interrupt, and maybe a wait function
;------------------------------------------------------------

;----------------------------------------------------------------
; Sub:	HitRight
; Desc:	Handles functionality of the TekBot when the right whisker
;		is triggered.
;----------------------------------------------------------------
HitRight:
		push	mpr									; Save mpr register
		push	waitcnt								; Save wait register
		in		mpr, SREG							; Save program state
		push	mpr			

		; clear consecLeft
		ldi mpr, $00								; place 0x00 into consecLeft
		mov ConsecLeft, mpr

		; increase consecRight
		mov mpr, ConsecRight
		inc mpr
		mov ConsecRight, mpr

		; Need to update HitRight Counter
		mov mpr, RightCount
		ldi waitcnt, $01							; use waitcnt because all other registers are being used
		add mpr, waitcnt							; increment rightCount value by 1
		mov RightCount, mpr							; place incremented value of rightcount into RightCount

		; check if double right hit
		mov mpr, ConsecRight
		cpi mpr, $2
		brlo PASS3
			;if greater than
			; Move Backwards for a second
		ldi		mpr, MovBck							; Load Move Backward command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL							; Load Turn Left Command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function
		rcall	WaitFunction						; Call wait function again

		; Move Forward again	
		ldi		mpr, MovFwd							; Load Move Forward command
		out		PORTB, mpr							; Send command to port

		;if this condition, it is known that alternating was not meet
		;therefore we must clear the alternating count
		ldi mpr, $00								; load $00 into alternating count
		mov alternating, mpr

		; update the interrupt flag
		ldi mpr, 0b00001111							; reset all of the INT flags INT0:INT3			
		out EIFR, mpr

		pop		mpr									; Restore program state
		out		SREG, mpr	
		pop		waitcnt								; Restore wait register
		pop		mpr									; Restore mpr
		ret											; Return from subroutine
		
		PASS3:






		; check if alternating is above 5
		mov mpr, alternating
		cpi mpr, $5
		brlo PASS7
			;if greater than
			; Move Backwards for a second
		ldi		mpr, MovBck							; Load Move Backward command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL							; Load Turn Left Command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function
		rcall	WaitFunction						; Call wait function again

		; Move Forward again	
		ldi		mpr, MovFwd							; Load Move Forward command
		out		PORTB, mpr							; Send command to port

		; update the interrupt flag
		ldi mpr, 0b00001111							; reset all of the INT flags INT0:INT3			
		out EIFR, mpr

		pop		mpr									; Restore program state
		out		SREG, mpr	
		pop		waitcnt								; Restore wait register
		pop		mpr									; Restore mpr
		ret											; Return from subroutine
		
		PASS7:



		;if this code is executing it means that it was not consequtive
		;therefore it is alternating
		inc alternating

		; Move Backwards for a second
		ldi		mpr, MovBck							; Load Move Backward command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function

		; Turn left for a second
		ldi		mpr, TurnL							; Load Turn Left Command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function

		; Move Forward again	
		ldi		mpr, MovFwd							; Load Move Forward command
		out		PORTB, mpr							; Send command to port



		; update the interrupt flag
		ldi mpr, 0b00001111							; reset all of the INT flags INT0:INT3			
		out EIFR, mpr

		pop		mpr									; Restore program state
		out		SREG, mpr	
		pop		waitcnt								; Restore wait register
		pop		mpr									; Restore mpr
		ret											; Return from subroutine

;----------------------------------------------------------------
; Sub:	HitLeft
; Desc:	Handles functionality of the TekBot when the left whisker
;		is triggered.
;----------------------------------------------------------------
HitLeft:
		push	mpr								; Save mpr register
		push	waitcnt							; Save wait register
		in		mpr, SREG						; Save program state
		push	mpr			

		; clear consecRight
		ldi mpr, $00							; put 0x00 into ConsecRight
		mov ConsecRight, mpr

		; increase consecLeft
		mov mpr, ConsecLeft
		inc mpr
		mov ConsecLeft, mpr

		; Need to update HitRight Counter
		mov mpr, LeftCount
		ldi waitcnt, $01						; use waitcnt because all other registers are being used
		add mpr, waitcnt						; increment LeftCount value by 1
		mov LeftCount, mpr						; place incremented value of left count into LeftCount


		; check if double right hit
		mov mpr, ConsecLeft
		cpi mpr, $2
		brlo PASS4
			;if greater than
			; Move Backwards for a second
		ldi		mpr, MovBck							; Load Move Backward command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function

		; Turn left for a second
		ldi		mpr, TurnR							; Load Turn Left Command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function
		rcall	WaitFunction						; Call wait function again

		; Move Forward again	
		ldi		mpr, MovFwd							; Load Move Forward command
		out		PORTB, mpr							; Send command to port

		;if this condition, it is known that alternating was not meet
		;therefore we must clear the alternating count
		ldi mpr, $00								; load $00 into alternating count
		mov alternating, mpr

		; update the interrupt flag
		ldi mpr, 0b00001111							; reset all of the INT flags INT0:INT3			
		out EIFR, mpr

		pop		mpr									; Restore program state
		out		SREG, mpr	
		pop		waitcnt								; Restore wait register
		pop		mpr									; Restore mpr
		ret											; Return from subroutine
		
		PASS4:


		;check if alternating is above 5
		mov mpr, alternating
		cpi mpr, $5
		brlo PASS6
		; Move Backwards for a second
		ldi		mpr, MovBck							; Load Move Backward command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function

		; Turn left for a second
		ldi		mpr, TurnR							; Load Turn Left Command
		out		PORTB, mpr							; Send command to port
		ldi		waitcnt, WTime						; Wait for 1 second
		rcall	WaitFunction						; Call wait function
		rcall	WaitFunction						; Call wait function again

		; Move Forward again	
		ldi		mpr, MovFwd							; Load Move Forward command
		out		PORTB, mpr							; Send command to port


		; update the interrupt flag
		ldi mpr, 0b00001111							; reset all of the INT flags INT0:INT3			
		out EIFR, mpr

		pop		mpr									; Restore program state
		out		SREG, mpr	
		pop		waitcnt								; Restore wait register
		pop		mpr									; Restore mpr
		ret											; Return from subroutine
		PASS6:


		;if this code is executing it means that it was not consequtive
		;therefore it is alternating
		inc alternating

		; Move Backwards for a second
		ldi		mpr, MovBck					; Load Move Backward command
		out		PORTB, mpr					; Send command to port
		ldi		waitcnt, WTime				; Wait for 1 second
		rcall	WaitFunction				; Call wait function

		; Turn right for a second
		ldi		mpr, TurnR					; Load Turn Left Command
		out		PORTB, mpr					; Send command to port
		ldi		waitcnt, WTime				; Wait for 1 second
		rcall	WaitFunction				; Call wait function
			
		; Move Forward again	
		ldi		mpr, MovFwd					; Load Move Forward command
		out		PORTB, mpr					; Send command to port

		; reset the interrupt flags
		ldi mpr, 0b00001111					; reset INT0:INT3
		out EIFR, mpr

		pop		mpr		; Restore program state
		out		SREG, mpr	;
		pop		waitcnt		; Restore wait register
		pop		mpr		; Restore mpr
		ret				; Return from subroutine

;----------------------------------------------------------------
; Sub:	Wait
; Desc:	A wait loop that is 16 + 159975*waitcnt cycles or roughly 
;		waitcnt*10ms.  Just initialize wait for the specific amount 
;		of time in 10ms intervals. Here is the general eqaution
;		for the number of clock cycles in the wait loop:
;			((3 * ilcnt + 3) * olcnt + 3) * waitcnt + 13 + call
;----------------------------------------------------------------
WaitFunction:
		push	waitcnt			; Save wait register
		push	ilcnt			; Save ilcnt register
		push	olcnt			; Save olcnt register

Loop:	ldi		olcnt, 224		; load olcnt register
OLoop:	ldi		ilcnt, 237		; load ilcnt register
ILoop:	dec		ilcnt			; decrement ilcnt
		brne	ILoop			; Continue Inner Loop
		dec		olcnt			; decrement olcnt
		brne	OLoop			; Continue Outer Loop
		dec		waitcnt			; Decrement wait 
		brne	Loop			; Continue Wait loop	

		pop		olcnt			; Restore olcnt register
		pop		ilcnt			; Restore ilcnt register
		pop		waitcnt			; Restore wait register
		ret						; Return from subroutine


;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
ClearRight:							; Begin a function with a label

		; Save variable by pushing them to the stack
		push mpr							; need to keep old value of mpr, so use push/pop

		; Execute the function here

		call LCDClrLn1						; clear the first line on the LCD

		; Need to update HitRight Counter to be 0
		mov mpr, RightCount					; put RightCount into mpr for manipulation
		ldi mpr, $30						; update value to be 0 ($30 is ascii key for 0)
		mov RightCount, mpr					; place cleared value of rightcount into RightCount

		ldi		waitcnt, WTime				; Wait for 1 second
		rcall	WaitFunction				; Call wait function

		; reset the interrupt flags
		ldi mpr, 0b00001111					; reset INT0:INT3
		out EIFR, mpr
		
		; Restore variable by popping them from the stack in reverse order
		pop mpr

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
ClearLeft:							; Begin a function with a label

		; Save variable by pushing them to the stack
		push mpr									; need to keep old value of mpr, so use push/pop

		; Execute the function here

		call LCDClrLn2								; clear the second line on the LCD

		; Need to update HitLeft Counter to be 0
		mov mpr, LeftCount							; put leftCount into mpr for manipulation
		ldi mpr, $30								; update value to be 0 ($30 is ascii for 0)
		mov LeftCount, mpr							; place cleared value of leftcount into LeftCount

		ldi		waitcnt, WTime				; Wait for 1 second
		rcall	WaitFunction				; Call wait function

		; reset the interrupt flags
		ldi mpr, 0b00001111					; reset INT0:INT3
		out EIFR, mpr
		
		; Restore variable by popping them from the stack in reverse order
		pop mpr

		ret						; End a function with RET




;***********************************************************
;*	Stored Program Data
;***********************************************************

.dseg							; allocate Data Memory
;.org $0150						;keep track of Right Hit
;RightCounter:
;	.byte 4

;.org $0160						; keep track of Left Hit
;LeftCounter:
;	.byte 4

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program