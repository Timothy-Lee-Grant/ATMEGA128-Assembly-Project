;***********************************************************
;*
;*	Morse Code Transmitter
;*
;*	Enter the description of the program here
;*
;*	This is the skeleton file for Lab 8 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Timothy Lee Grant
;*	   Date: 23 Nov, 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register

.def counter = r23
.def numberOfChar = r24



;.equ	EngEnR = 4				; right Engine Enable Bit

;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; beginning of code segment

;***********************************************************
;*	Interrupt Vectors
;***********************************************************
.org	$0000
		rjmp	INIT			; reset interrupt

		; place instructions in interrupt vectors here, if needed

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

		; Initialize the LCD Driver
		call LCDInit
		call LCDClrLn1
		call LCDClrLn2

		; Configure I/O ports

		ldi mpr, 0b00000000			;configure PortD as an input
		out DDRD, mpr
		ldi mpr, $FF
		out PORTD, mpr

		ldi mpr, $FF				;configure portB as output
		out DDRB, mpr
		ldi mpr, $00				; initially all leds are off
		out PORTB, mpr

		; Configure External Interrupts, if needed

		; Configure 16-bit Timer/Counters
		; 0bxxx01xxx TCCR1B (for CTC Mode)
		; 0bxxxxx101 TCCR1B (for 1024 prescale)
		ldi mpr, 0b00001101		; CTC Wave generation, and prescale of 1024
		out TCCR1B, mpr

		; 0bxxxxxx00 TCCR1A (for CTC Mode)

		ldi mpr, 0b00000000		; normal mode
		out TCCR1A, mpr

		ldi mpr, 0b00000000		; enable Timer/counter output comare A match interrupt 
		out TIMSK, mpr
										
		; Enable global interrupts (if any are used)
		sei

		ldi numberOfChar, $0	; numberOfChar is initially 0


		
		;initial welcome screen
		push r16
		push XH
		push XL

		ldi ZH, high(WelcomeMessage1<<1)		;load the first welcome message on the first line
		ldi ZL, low(WelcomeMessage1<<1)			;Z will point to the location where the welcome message is
		ldi XH, $01								;X points to the location in data memory that the string needs to be stored
		ldi XL, $00

		ldi r17, $8								;8 total caracters

		WelcomeLoop1:							;keep looping until all characters are placed in data memory
			lpm r16, Z+							;load from where Z points and put it where LCD will read
			st X+, r16
			dec r17
			brne WelcomeLoop1

		ldi ZH, high(WelcomeMessage2<<1)		;the second welcome message goes on the second line of the LCD
		ldi ZL, low(WelcomeMessage2<<1)
		ldi XH, $01								;$0110 is the location in data memory of the second line of LCD
		ldi XL, $10
		
		ldi r17, $10							;16 total letter in total for the second welcome message

		WelcomeLoop2:							;keep looping until entire second welcome message is loaded in data mem
			lpm r16, Z+
			st X+, r16
			dec r17
			brne WelcomeLoop2

		call LCDWrite
		WelcomeLoop3:							;keep looping until the user presses PD0
			in mpr, PIND						;check the status of port D (input)
			andi mpr, 0b00000001				;if PD0 is pressed, break out of loop
			brne WelcomeLoop3
			
		pop XL
		pop XH
		pop r16

		call LCDClrLn1							;clear the LCD screen of the previous message
		call LCDClrLn2

		;load the standard message that will always be displayed
		ldi ZH, high(StandardMessage<<1)		;load the standard message address into the Z pointer
		ldi ZL, low(StandardMessage<<1)
		ldi XH, $01								;the standard message should be on the first line of the LCD
		ldi XL, $00
		ldi r17, 11
		StandardLoop1:			;load entire string of first welcome message
			lpm r16, Z+			;load from program memory and store in location where LCD will print
			st X+, r16
			dec r17
			brne StandardLoop1	;branch until all characters are in data memory
		

		ldi ZH, $01				;address 0100 is placed into Z register
		ldi ZL, $10
		ldi mpr, $41			; load A into first location
		st Z, mpr

		call LCDWrite			; write A to the screen
		rcall WaitFunction		; So that A is not immediately confimed


;***********************************************************
;*	Main Program
;***********************************************************
MAIN:
		; poll Port D pushbuttons (if needed)

								; if pressed, adjust speed
								; also, adjust speed indication

		ldi counter, $0			;at the start A is counted as 1
		
		LOOP:
			in mpr, PIND
			sbrs mpr, 7			; pin 7 pressed, call Reverse Order
			call ReverseOrder

			in mpr, PIND
			sbrs mpr, 6			; pin 6 pressed call Forward Order
			call ForwardOrder

			in mpr, PIND		;pin 0 to confirm letter
			sbrs mpr, 0
			call ConfirmLetter

			in mpr, PIND		;pin 4 to output the characters to the LED
			sbrs mpr, 4
			rcall WriteMorseCode

			cpi numberOfChar, 16	;check if user has confirmed 16 characters
			brne Skip22
			rcall WriteMorseCode	;once user enters 16 characters, automatically call WriteMorseCode
			Skip22:
			
			call LCDWrite		; keep updating the screen each itteration


			rjmp LOOP


		rjmp	MAIN			; return to top of MAIN

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

WriteMorseCode:

	rcall OneSecondWait							; one second delay for debouncing	

	; Z to point to the first character to transmit
	ldi ZH, $01
	ldi ZL, $10

	;check if no confirmed
	cpi numberOfChar, 1
	brlt Done3

	;at this point numberOfChar contains the total number of characters
	;keep itterating until numberOfChar == 0
	Loop4:
		rcall FindLetter						; function finds and prints corresponding morse code
		ldi mpr, 0								; load 0 value into location (to represent null character) ie empty
		st Z+, mpr								; increase Z to point to next location
		dec numberOfChar						; decrease number of chars (when 0 quit looping)
		brne LOOP4

	;load 0 into the (unconfirmed character)
	ldi mpr, 0
	st Z, mpr
	rcall LCDClrLn2								; clear the screen

	;reset Z to point to start of first address in LCD screen
	ldi ZH, $01
	ldi ZL, $10
	ldi mpr, $41								; load A into first location
	st Z, mpr

	;reset counter to 0
	ldi numberOfChar, 0							;technically not needed, but helps for clarity

	;what do I do about the counter?

	Done3:
	ret

ConfirmLetter:
	ld mpr, Z+					; increase Z to the next position
	ldi mpr, $41				;41 is hex A
	st Z, mpr
	ldi counter, 0				;counter set to 0, to represent the first letter
	inc numberOfChar			;numberOfChar increased by 1 because letter is now confirmed

	push r16
	ldi r16, 50
	rcall WaitFunction			;wait
	pop r16

	ret


ForwardOrder:

	inc counter					;increase the counter by 1 to represnt that the letter is the next in the ascii
	cpi counter, 26				;compare to see if current value is larger than ascii Z (if so wrap around)
	brge WrapAround1

	ld mpr, Z					;if not larger, increase the value located at Z by 1 
	inc mpr
	st Z, mpr					;store value back at Z
	jmp Done1					;dont wrap around

	;overflow too high
	WrapAround1:
		ldi counter, $0			;counter is now at 0 to represent at the first letter A
		ldi mpr, $41			;41 is hex A
		st Z, mpr				;load A into location where Z is pointing

	Done1:

	push r16
	ldi r16, 50
	rcall WaitFunction			;debounce wait
	pop r16

	ret


ReverseOrder:

	dec counter					;decrease the counter by 1 to represnt that the letter is the previous in the ascii
	cpi counter, 0				;compare to see if value is less than 0. If less a wrap around is required
	brlt WrapAround2

	ld mpr, Z					;if not less, decrease the ascii value by 1 and return
	dec mpr
	st Z, mpr					;store the decremented value back at where Z points
	jmp Done2					;jump to done

	;overflow too high
	WrapAround2:
		ldi counter, 25		; I think without anything represents decimal 26
		ldi mpr, $5A		;41 is hex A
		st Z, mpr

	Done2:

	push r16
	ldi r16, 50
	rcall WaitFunction			;wait for debouncing
	pop r16
	ret


WriteToScreen:

;*			- Line 1 data is in address space $0100-$010F
;*			- Line 2 data is in address space $0110-$010F
	ldi ZH, $01
	ldi ZL, $00
	ldi mpr, $47
	st Z, mpr

	call LCDWrite
	
	ret



;150ms wait for debouncing purposes
;0x086F = 2159d (just under 150ms)
WaitFunctionNotWorking:
	push mpr

	;load OCR with value
	ldi mpr, $33								; load the value to count to
	out OCR1AH, mpr								; high register first
	ldi mpr, $A0								; low byte second
	out OCR1AL, mpr								; load low byte into OCR(1A)
	;load TCNT with start value
	ldi mpr, 0									; TCNT contains the start value
	out TCNT1H, mpr								; load 0 into TCNTH to start at 0
	out TCNT1L, mpr								; load 0 into TCNTL to start at 0

	;check if OCF is set
	BEGIN10:										; keep looping until reaching OCF flag triggered
		in mpr, TIFR							; read in OCF1A from TIFR register
		andi mpr, 0b00010000					; check OFC1A, if not set keep looping
		breq BEGIN10								; if not set keep looping
	;clear OCF in TIFR (by writing a 1 to bit)
	ldi mpr, 0b00010000							; write 1 to OFC1A to clear flag
	out TIFR, mpr								; clear the OCF1A in TIFR register

	pop mpr

	ret




WaitFunction:
		push r16
		push r17
		push r18

		LoopTemp:
			ldi r18, 100						;original value 224 (150 works)
		OLoop:
			ldi r17, 237						;original value 237
		ILoop:
			dec r17
			brne ILoop
			dec r18
			brne OLoop
			dec r16
			brne LoopTemp

		pop r18
		pop r17
		pop r16
		ret

;clock speed 16*10^6Hz
;need 43200 (0xB71B)
ThreeSecondWait:
	push mpr

	;load OCR with value
	ldi mpr, $B7								; load the value to count to
	out OCR1AH, mpr								; high register first
	ldi mpr, $1B								; low byte second
	out OCR1AL, mpr								; load low byte into OCR(1A)
	;load TCNT with start value
	ldi mpr, 0									; TCNT contains the start value
	out TCNT1H, mpr								; load 0 into TCNTH to start at 0
	out TCNT1L, mpr								; load 0 into TCNTL to start at 0

	;check if OCF is set
	BEGIN2:										; keep looping until reaching OCF flag triggered
		in mpr, TIFR							; read in OCF1A from TIFR register
		andi mpr, 0b00010000					; check OFC1A, if not set keep looping
		breq BEGIN2								; if not set keep looping
	;clear OCF in TIFR (by writing a 1 to bit)
	ldi mpr, 0b00010000							; write 1 to OFC1A to clear flag
	out TIFR, mpr								; clear the OCF1A in TIFR register

	pop mpr

	ret

;clock at 16*10^6 MHz
;need 15625 (0x3D09) to get 1 second
OneSecondWait:
	push mpr

	;load OCR with value
	ldi mpr, $3D								; load the value to count to
	out OCR1AH, mpr								; high register first
	ldi mpr, $09								; low byte second
	out OCR1AL, mpr								; load low byte into OCR(1A)
	;load TCNT with start value
	ldi mpr, 0									; TCNT contains the start value
	out TCNT1H, mpr								; load 0 into TCNTH to start at 0
	out TCNT1L, mpr								; load 0 into TCNTL to start at 0

	;check if OCF is set
	BEGIN3:										; keep looping until reaching OCF flag triggered
		in mpr, TIFR							; read in OCF1A from TIFR register
		andi mpr, 0b00010000					; check OFC1A, if not set keep looping
		breq BEGIN3								; if not set keep looping
	;clear OCF in TIFR (by writing a 1 to bit)
	ldi mpr, 0b00010000							; write 1 to OFC1A to clear flag
	out TIFR, mpr								; clear the OCF1A in TIFR register

	pop mpr
	ret

between:
	;turn off LED
	ldi mpr, $00
	out PINB, mpr
	rcall OneSecondWait
	ret
newChar:
	;turn off LED
	ldi mpr, $00
	out PINB, mpr
	rcall ThreeSecondWait
	ret

dash:
	;turn on LED Lights
	ldi mpr, 0b11100000
	out PORTB, mpr
	rcall ThreeSecondWait
	;turn off LED
	ldi mpr, $00
	out PORTB, mpr
	ret

dot:
	;turn on LED Lights
	ldi mpr, 0b11100000
	out PORTB, mpr
	rcall OneSecondWait
	;turn off LED
	ldi mpr, $00
	out PORTB, mpr
	ret


;  Z pointer will be pointing to the character needing to be printed
; letter will be ascii 
FindLetter:
	
	ld mpr, Z					; load the current letter ascii value into mpr

	;A
	cpi mpr, $41				; check if value is A
	brne next1					; if not equal, branch to the next
	rcall A						; if is equal call A function
	ret

	;B
	next1:
	cpi mpr, $42
	brne next2
	rcall B
	ret

	next2:
	cpi mpr, $43
	brne next3
	rcall C
	ret

	next3:
	cpi mpr, $44
	brne next4
	rcall D
	ret

	next4:
	cpi mpr, $45
	brne next5
	rcall E
	ret

	next5:
	cpi mpr, $46
	brne next6
	rcall F
	ret

	next6:
	cpi mpr, $47
	brne next7
	rcall G
	ret

	next7:
	cpi mpr, $48
	brne next8
	rcall H
	ret

	next8:
	cpi mpr, $49
	brne next9
	rcall I
	ret

	next9:
	cpi mpr, $4A
	brne next10
	rcall J
	ret

	next10:
	cpi mpr, $4B
	brne next11
	rcall K
	ret

	next11:
	cpi mpr, $4C
	brne next12
	rcall L
	ret

	next12:
	cpi mpr, $4D
	brne next13
	rcall M
	ret

	next13:
	cpi mpr, $4E
	brne next15
	rcall N
	ret

	next15:
	cpi mpr, $4F
	brne next16
	rcall O
	ret

	next16:
	cpi mpr, $50
	brne next17
	rcall P
	ret

	next17:
	cpi mpr, $51
	brne next18
	rcall QLet
	ret

	next18:
	cpi mpr, $52
	brne next19
	rcall RLet
	ret

	next19:
	cpi mpr, $53
	brne next20
	rcall S
	ret

	next20:
	cpi mpr, $54
	brne next21
	rcall T
	ret

	next21:
	cpi mpr, $55
	brne next22
	rcall U
	ret

	next22:
	cpi mpr, $56
	brne next23
	rcall V
	ret

	next23:
	cpi mpr, $57
	brne next24
	rcall W
	ret

	next24:
	cpi mpr, $58
	brne next25
	rcall XLet
	ret

	next25:
	cpi mpr, $59
	brne next26
	rcall YLet
	ret

	next26:
	cpi mpr, $5A
	rcall ZLet
	ret





	ret


A:
	;dot, dash
	rcall dot
	rcall between
	rcall dash

	;new character
	rcall newChar
	ret

B:
	; dash dot dot dot
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dot

	rcall newChar
	ret

C:
	; dash dot dash dot
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dot

	rcall newChar
	ret

D:
	;dash dot dot
	rcall dash
	rcall between
	rcall dot 
	rcall between
	rcall dot

	rcall newChar
	ret

E:
	;dot
	rcall dot

	rcall newChar
	ret

F:
	; dot dot dash dot
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dot

	rcall newChar
	ret

G:
	; dash dash dot
	rcall dash
	rcall between
	rcall dash
	rcall between
	rcall dot

	rcall newChar
	ret

H:
	;dot dot dot dot
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dot

	rcall newChar
	ret

I:
	; dot dot
	rcall dot
	rcall between
	rcall dot

	rcall newChar
	ret

J:
	;dot dash dash dash
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dash
	rcall between
	rcall dash

	rcall newChar
	ret

K:
	;dash dot dash
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dash

	rcall newChar
	ret

L: 
	; dot dash dot dot
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dot

	rcall newChar
	ret

M: 
	;dash dash
	rcall dash
	rcall between
	rcall dash

	rcall newChar
	ret

N:
	; dash dot
	rcall dash
	rcall between
	rcall dot

	rcall newChar
	ret

O:
	;dash dash dash
	rcall dash
	rcall between
	rcall dash
	rcall between
	rcall dash

	rcall newChar
	ret

P:
	;dot dash dash dot
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dash
	rcall between
	rcall dot

	rcall newChar
	ret

;q already defined as r21
QLet:
	;dash dash dot dash
	rcall dash
	rcall between
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dash

	rcall newChar
	ret

RLet:
	; dot dash dot
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dot

	rcall newChar
	ret

S:
	;dot dot dot
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dot

	rcall newChar
	ret

T:
	;dash
	rcall dash

	rcall newChar
	ret

U:
	; dot dot dash
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dash

	rcall newChar
	ret

V:
	;dot dot dot dash
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dash

	rcall newChar
	ret

W:
	;dot dash dash
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dash

	rcall newChar
	ret

XLet:
	;dash dot dot dash
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dot
	rcall between
	rcall dash

	rcall newChar
	ret

YLet:
	;dash dot dash dash
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dash
	rcall between
	rcall dash

	rcall newChar
	ret

ZLet:
	;dash dash dot dot
	rcall dash
	rcall between
	rcall dash
	rcall between
	rcall dot
	rcall between
	rcall dot

	rcall newChar
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
WelcomeMessage1:
	.db		"Welcome!"

WelcomeMessage2:
	.db		"Please Press PD0"

StandardMessage:
	.db		"Enter Word:"

;***********************************************************
;*	Additional Program Includes
;***********************************************************

.include "LCDDriver.asm"		; include the LCD Driver

		; There are no additional file includes for this program