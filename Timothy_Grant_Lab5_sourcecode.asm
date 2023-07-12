

;***********************************************************
;*
;*	Lab 5
;*
;*	Arithmatic operations in AVR
;*
;*	This is the skeleton file for Lab 5 of ECE 375
;*
;***********************************************************
;*
;*	 Author: Timothy Grant	
;*	   Date: Oct 29, 2021
;*
;***********************************************************

.include "m128def.inc"			; Include definition file

;***********************************************************
;*	Internal Register Definitions and Constants
;***********************************************************
.def	mpr = r16				; Multipurpose register 
.def	rlo = r0				; Low byte of MUL result
.def	rhi = r1				; High byte of MUL result
.def	zero = r2				; Zero register, set to zero in INIT, useful for calculations
.def	A = r3					; A variable
.def	B = r4					; Another variable

;;;;;;;;;; oloop was origionally r17, iloop was origionally r18
;;;;;;;;;; changed to allow for continuous register of operands

.def	oloop = r21				; Outer Loop Counter
.def	iloop = r22				; Inner Loop Counter


;***********************************************************
;*	Start of Code Segment
;***********************************************************
.cseg							; Beginning of code segment

;-----------------------------------------------------------
; Interrupt Vectors
;-----------------------------------------------------------
.org	$0000					; Beginning of IVs
		rjmp 	INIT			; Reset interrupt

.org	$0046					; End of Interrupt Vectors

;-----------------------------------------------------------
; Program Initialization
;-----------------------------------------------------------
INIT:							; The initialization routine
		; Initialize Stack Pointer
		; TODO					; Init the 2 stack pointer registers
		ldi mpr, high(RAMEND)
		out SPH, mpr				;initilize the stack pointer high byte
		ldi mpr, low(RAMEND)
		out SPL, mpr				;initilize the stack pointer low byte

		clr		zero			; Set the zero register to zero, maintain
								; these semantics, meaning, don't
								; load anything else into it.

;-----------------------------------------------------------
; Main Program
;-----------------------------------------------------------
MAIN:							; The Main program
		; Setup the ADD16 function direct test

				; Move values 0xFCBA and 0xFFFF in program memory to data memory
				; memory locations where ADD16 will get its inputs from
				; (see "Data Memory Allocation" section below)

				ldi		ZL, low(ADD16_PM1<<1)	; Load low byte of address
				ldi 	ZH, high(ADD16_PM1<<1)	; Load high byte of address

				lpm XL, Z+					; load the contents at location Z into XL
				lpm XH, Z					; load the contents at location Z high into XH

				ldi YL, $10					;location to store operands of addition
				ldi YH, $01
				st Y+, XL					; place low byte of first operand into $0100
				st Y+, XH					; place high byte of first operand into $0101

				; Load second operand
				ldi ZL, low(ADD16_PM2<<1)		; load low byte of second operand address
				ldi ZH, high(ADD16_PM2<<1)		; load high byte of second operand address

				lpm XL, Z+					; load the contents at location Z into XL
				lpm XH, Z					; load the contents at location Z high into XH

				st Y+, XL					; place low byte of second operand into $0102
				st Y+, XH					; place high byte of second operand into $0103

                nop ; Check load ADD16 operands (Set Break point here #1)  
				; Call ADD16 function to test its correctness
				; (calculate FCBA + FFFF)
				call ADD16

                nop ; Check ADD16 result (Set Break point here #2)
				; Observe result in Memory window

		; Setup the SUB16 function direct test

			; Move values 0xFCB9 and 0xE420 in program memory to data memory
			; memory locations where SUB16 will get its inputs from
			ldi ZL, low(SUB16_PM1<<1)
			ldi ZH, high(SUB16_PM1<<1)

			lpm XL, Z+
			lpm XH, Z						; 
											; X now contains the value that was in program memory

			ldi YL, $30						; Y points to $0130, an arbitrary point in data memory which the operands of the SUB will be stored
			ldi YH, $01

			st Y+, XL						; store low byte first operand at $0130
			st Y+, XH						; store high byte of first operand at $0131
											; Y now points to the low byte of the second operand in SUB

			ldi ZL, low(SUB16_PM2<<1)
			ldi ZH, high(SUB16_PM2<<1)

			lpm XL, Z+						; load the second operand in PM to X register
			lpm XH, Z						; load high byte of operand

			st Y+, XL						; store the low byte of second operand at $0132
			st Y, XH						; store high byte of second operand at $0133

				 nop	; Check load SUB16 operands (Set Break point here #3)  
						; Call SUB16 function to test its correctness
						; (calculate FCB9 - E420)
			call SUB16

				  nop	; Check SUB16 result (Set Break point here #4)
						; Observe result in Memory window


		; Setup the MUL24 function direct test

				; Move values 0xFFFFFF and 0xFFFFFF in program memory to data memory  
				; memory locations where MUL24 will get its inputs from
				ldi ZL, low(MUL24_PM1<<1)
				ldi ZH, high(MUL24_PM1<<1)

				lpm XL, Z+						; load the first operand into X register
				lpm XH, Z+

				ldi YL, $50						; address to load the operands
				ldi YH, $01

				st Y+, XL						; store the low byte at $0150 data memory
				st Y+, XH						; store high byte of first operand at $0151
				
				lpm XL, Z						; loads third byte of operand into XL
				st Y, XL						; load third byte into 0152


				ldi ZL, low(MUL24_PM2<<1)		; load the second operand address into the Z register
				ldi ZH, high(MUL24_PM2<<1)

				lpm XL, Z+						; load the value of second operand into X
				lpm XH, Z

				ldi YL, $60						; point Y to the location in data memory for second operand
				ldi YH, $01

				st Y+, XL						; store low byte into data memory ($0160)
				st Y, XH						; store high byte into data memory ($0161)

				lpm XL, Z						; loads third byte of operand into XL
				st Y, XL						; load third byte into ($0162)




                nop ; Check load MUL24 operands (Set Break point here #5)  
				; Call MUL24 function to test its correctness
				; (calculate FFFFFF * FFFFFF)

				

				call MUL24
                nop ; Check MUL24 result (Set Break point here #6)
				; Observe result in Memory window




                nop ; Check load COMPOUND operands (Set Break point here #7)  
		; Call the COMPOUND function

				ldi ZL, low(OperandD<<1)		; load the location in Program memory to the Z pointer
				ldi ZH, high(OperandD<<1)

				lpm XL, Z+						; load the OperandD into X register
				lpm XH, Z+

				ldi YL, $A0						; address to load the operands
				ldi YH, $01

				st Y+, XL						; store the low byte at $01A0 data memory
				st Y+, XH						; store high byte of first operand at $01A1

				;load operand E
				ldi ZL, low(OperandE<<1)
				ldi ZH, high(OperandE<<1)

				lpm XL, Z+						; load low byte of operand E into XL
				lpm XH, Z+						; load high byte of operand E into XH

				ldi YL, $A2						; operand E will go to $01A2:$01A3
				ldi YH, $01

				st Y+, XL						; place the values into data memory at location $01A2:$01A3
				st Y+, XH

				; load operand F
				ldi ZL, low(OperandF<<1)
				ldi ZH, high(OperandF<<1)

				lpm XL, Z+						; load low byte of operand F into XL
				lpm XH, Z+						; load high byte of operand F into XH

				ldi YL, $A4						; operand F will go to $01A4:$01A5
				ldi YH, $01

				st Y+, XL						; place the values into data memory at location $01A4:$01A5
				st Y+, XH

				call COMPOUND

                nop ; Check COMPUND result (Set Break point here #8)
				; Observe final result in Memory window

				

DONE:	rjmp	DONE			; Create an infinite while loop to signify the 
								; end of the program.

;***********************************************************
;*	Functions and Subroutines
;***********************************************************

;-----------------------------------------------------------
; Func: ADD16
; Desc: Adds two 16-bit numbers and generates a 24-bit number
;		where the high byte of the result contains the carry
;		out bit.
;-----------------------------------------------------------
ADD16:
		; Load beginning address of first operand into X
		ldi XL, $10
		ldi XH, $01

		; Load beginning address of second operand into Y
		ldi YL, $12
		ldi YH, $01

		; Load beginning address of result into Z
		ldi ZL, $20
		ldi ZH, $01

		; Execute the function
		ld r16, X+					; load the value at memory location X into r16
		ld r17, Y+					; load the value at memory location Y into r17

		add r17, r16				; add (without carry) with the result in r17
		st Z+, r17					; store the result of addition of the low byte of the two operands at the location at memory which Z points to

		ld r16, X+					; load the next value that the updated memory location X points to into r16 (second byte of first operand)
		ld r17, Y+					; load the value at the incremented Y location into r17 (this points to the second byte of the second operand)

		adc r17, r16				; add with carry will add the two numbers, plus the carry from the previous operation
		st Z+, r17					; store the result in the memory location that Z points to

		ldi r17, $00				; clear the final carry location
		st Z, r17					; put 0 into the third result register
		brcc EXIT					; if the carry is cleared return
		ldi r17, $01
		st Z, r17					; if carry set put $01 in third result register (lab example video does st Z, XH instead)


		EXIT:
			ret						; End a function with RET

;-----------------------------------------------------------
; Func: SUB16
; Desc: Subtracts two 16-bit numbers and generates a 16-bit
;		result.
;-----------------------------------------------------------
SUB16:
		; Execute the function here

		ldi XL, $30					; X points to first operand (in data memory)
		ldi XH, $01

		ldi YL, $40					; Y points to data memory result location
		ldi YH, $01
		
		ld r16, X+					; load low byte of first operand into r16
		ld r17, X+					; load high byte of first operand into r17
		ld r18, X+					; load low bte of second operand into r18
		ld r19, X					; load high byte of second operand into r19

		SUB r16, r18				; low(op1) - low(op2) -> r16   (the low byte of operand 2 is subtracted from the low byte of operand 1, and then stored in r16
		st Y+, r16					; store the low byte of the operation ($0140)
		SBC r17, r19				; high(op1) - high(op2) - CarryBit -> r17   (subtracts with a carry bit)
		st Y, r17					; store the result in the high byte in data memory ($0141)

		ret							; End a function with RET

;-----------------------------------------------------------
; Func: MUL24
; Desc: Multiplies two 24-bit numbers and generates a 48-bit 
;		result.
;-----------------------------------------------------------
MUL24:
		; Execute the function here

		;multiply first two bytes of both operands
		;;load values into 0150, 0151 and 0160, 0161
		ldi XL, $50
		ldi XH, $01
		ld r20, X+				; r20 stores the lowest byte of the first operand 
		ld r21, X+				; r21 stores the middle byte of the first operand
		ld r22, X+				; r22 stores the highest byte of the first operand
		
		;load the second operand into r23, r24, and r25
		ldi XL, $60				; second operand located at $0160
		ldi XH, $01
		ld r23, X+				; r23 stores the lowest byte of the second operand
		ld r24, X+				; r24 stores the middle byte of the second operand
		ld r25, X+				; r25 stores the highest byte of the second operand

		mul r23, r20			; multiply and store in r00:r01
		mov r19, r0				; place the low byte of the operation into r19
		mov r2, r1				; place high byte of operation into r2

		mul r23, r21			; multiply low byte of second operand with middle byte of first operand
		mov r3, r0				; place the low byte of the operation into r3
		mov r4, r1				; place the high byte of the operation into r4

		;update comments below
		mul r23, r22			; multiply low byte of second operand with high byte of first operand
		mov r5, r0				; place the low byte of the operation into r3
		mov r6, r1				; place the high byte of the operation into r4

		;multiply middle byte of second operand (located in r24)
		mul r24, r20			; multiply low byte of second operand with high byte of first operand
		mov r7, r0				; place the low byte of the operation into r3
		mov r8, r1				; place the high byte of the operation into r4

		mul r24, r22			; multiply low byte of second operand with high byte of first operand
		mov r11, r0				; place the low byte of the operation into r3
		mov r12, r1				; place the high byte of the operation into r4

		;multiply the high byte of the second opeand (located in r25)
		mul r25, r20			; multiply low byte of second operand with high byte of first operand
		mov r13, r0				; place the low byte of the operation into r3
		mov r14, r1				; place the high byte of the operation into r4

		mul r25, r21			; multiply low byte of second operand with high byte of first operand
		mov r15, r0				; place the low byte of the operation into r3
		mov r16, r1				; place the high byte of the operation into r4

		mul r25, r22			; multiply low byte of second operand with high byte of first operand
		mov r17, r0				; place the low byte of the operation into r3
		mov r18, r1				; place the high byte of the operation into r4


		; all operations have been performed. 
		; now add the rusults as appropraite to multiplaction

		ldi XL, $70				; result of multiplcation should be stored at $0170
		ldi XH, $01

		st X+, r19				; stores lowest byte into data memory
								; now free to use r19 as tracker for carry bits of next byte


		ldi r26, $00			; put all 0s into r26 (r26 will be used to clear registers)
		mov r19, r26			; clear carry tracker
		mov r20, r26			; load 0 into dummy register (to do adc operations for carry tracker)
		add r2, r3				; add results of second byte 
		adc r19, r20			; places carry bit into r19

		add r2, r7
		adc r19, r20			; add another carry bit to result

		st X+, r2				; place second byte of result

		;next byte will accumulate in r4 and carry tracker will go into r2
		mov r2, r26				; r2 will be carry tracker for third byte of result
		mov r3, r26				; clear out dummy register
		add r4, r19				; add carry byte from previous byte 
		adc r2, r3				; place carry bit into r2
		add r4, r5
		adc r2, r3				; place carry bit into r2
		add r4, r8
		adc r2, r3				; place carry bit into r2
		add r4, r9
		adc r2, r3				; place carry bit into r2
		add r4, r13
		adc r2, r3				; place carry bit into r2
		st X+, r4				; store result of third byte into data memory

		; next byte will accumulate in r6 and carry tracker will go into r4
		mov r4, r26				; r4 will be the carry tracker for the fourth byte
		mov r3, r26				; dummy variable
		add r6, r2				; add carry tracker for previous byte
		adc r4, r3				; add carry bit to r4
		add r6, r10				; 
		adc r4, r3				; add carry bit to r4
		add r6, r11				; 
		adc r4, r3				; add carry bit to r4
		add r6, r14				; 
		adc r4, r3				; add carry bit to r4
		add r6, r15				; 
		adc r4, r3				; add carry bit to r4
		st X+, r6				; store result of fourth byte into data memory

		; next byte (fifth) will accumulate in r12 and the carry tracker will go into r8
		
		mov r8, r26				; clear carry tracker
		mov r7, r26				; dummy register
		add r12, r4				; add carry tracker from previous byte 
		adc r8, r7				; add carry bit to r8
		add r12, r16
		adc r8, r7				; add carry bit to r8 
		add r12, r17
		adc r8, r7				; add carry bit to r8
		st X+, r12				; store result of fifth byte into data memory

		; next byte (sixth) will accumulate in r18 and the carry tracker will go into r10
		mov r10, r26			; clear carry tracker 
		mov r9, r26				; dummy register
		add r18, r8				; add carry from previous byte
		adc r10, r9				; add carry into r10
		st X+, r18

		; next byte has no variables only the result of the carry tracker from previous byte
		st X, r10


		ret						; End a function with RET

;-----------------------------------------------------------
; Func: COMPOUND
; Desc: Computes the compound expression ((D - E) + F)^2
;		by making use of SUB16, ADD16, and MUL24.
;
;		D, E, and F are declared in program memory, and must
;		be moved into data memory for use as input operands.
;
;		All result bytes should be cleared before beginning.
;-----------------------------------------------------------
COMPOUND:

		; Setup SUB16 with operands D and E
		; Perform subtraction to calculate D - E

			; SUB16 takes operand in data memory location $0130 and $0132
			ldi ZL, $A0					;load operand D address into register Z
			ldi ZH, $01

			ld XL, Z+					; load operandD into register X
			ld XH, Z+

			ldi ZL, $30				; $0130 data memory location that SUB first operand
			ldi ZH, $01

			st Z+, XL				; place operandD into first operand of SUB16
			st Z, XH


			ldi ZL, $A2					;load operand E address into register Z
			ldi ZH, $01

			ld XL, Z+					; load operandE into register X
			ld XH, Z+

			ldi ZL, $32				; $0132 data memory location that SUB16 second operand
			ldi ZH, $01

			st Z+, XL				; place operandE into second operand of SUB16
			st Z, XH

			call SUB16				; result will be stored in data memory location $0140:$0141
		
		; Setup the ADD16 function with SUB16 result and operand F
		; Perform addition next to calculate (D - E) + F

			ldi ZL, $A4					;load operandF address into register Z
			ldi ZH, $01

			ld XL, Z+					; load operandD into register X
			ld XH, Z+

			ldi ZL, $10					; $0110 data memory location that ADD16 first operand
			ldi ZH, $01

			st Z+, XL					; place operandF into first operand of SUB16
			st Z, XH

			;get result from previous SUB16 operation and store in second operand location for ADD16
			ldi ZL, $40					; loaction of result from SUB16 is $0140
			ldi ZH, $01

			ld XL, Z+					; load data from data memory of result from SUB16 into X register
			ld XH, Z+

			ldi ZL, $12					; load result of SUB16 into data memory of second operand of ADD16 ($0112)
			ldi ZH, $01

			st Z+, XL					; store the value of the SUB16 result into data memory of ADD16 second operation
			st Z, XH

			call ADD16					; call ADD16 function

		; Setup the MUL24 function with ADD16 result as both operands
		; Perform multiplication to calculate ((D - E) + F)^2

			;get the result from the ADD16 operation and multiply it by itself
			; ADD16 result is stored at $0120:$0122 in data memory 

			ldi ZL, $20					; Z points to location of result from ADD16
			ldi ZH, $01

			ld XL, Z+					; load result of ADD16 into X register
			ld XH, Z+

			ldi YL, $50					; first operand of MUL16 located at $0150
			ldi YH, $01

			st Y+, XL					; put first two bytes of first operand
			st Y+, XH

			ld XL, Z					; load third byte of first operand into XL register
			st Y, XL					; store third byte of first operand into $0152

			; load second operand for MUL24
			ldi ZL, $20					; Z points to location of result from ADD16
			ldi ZH, $01

			ld XL, Z+					; load result of ADD16 into X register
			ld XH, Z+

			ldi YL, $60					; second operand of MUL16 located at $0160
			ldi YH, $01

			st Y+, XL					; put first two bytes of second operand
			st Y+, XH

			ld XL, Z					; load third byte of first operand into XL register
			st Y, XL					; store third byte of first operand into $0152

			call MUL16

			ldi ZL, $70					; result of MUL24 is $0170
			ldi ZH, $01

			ldi YL, $B0					; location of the compound function is $01B0:$01B5
			ldi YH, $01

			ld XL, Z+					; following block of code loads two bytes from the result of mutiplication and loads it into result of compound function
			ld XH, Z+
			st Y+, XL
			st Y+, XH

			ld XL, Z+					; following block of code loads two bytes from the result of mutiplication and loads it into result of compound function
			ld XH, Z+
			st Y+, XL
			st Y+, XH

			ld XL, Z+					; following block of code loads two bytes from the result of mutiplication and loads it into result of compound function
			ld XH, Z+
			st Y+, XL
			st Y+, XH

		ret						; End a function with RET

;-----------------------------------------------------------
; Func: MUL16
; Desc: An example function that multiplies two 16-bit numbers
;			A - Operand A is gathered from address $0101:$0100
;			B - Operand B is gathered from address $0103:$0102
;			Res - Result is stored in address 
;					$0107:$0106:$0105:$0104
;		You will need to make sure that Res is cleared before
;		calling this function.
;-----------------------------------------------------------
MUL16:
		push 	A				; Save A register
		push	B				; Save B register
		push	rhi				; Save rhi register
		push	rlo				; Save rlo register
		push	zero			; Save zero register
		push	XH				; Save X-ptr
		push	XL
		push	YH				; Save Y-ptr
		push	YL				
		push	ZH				; Save Z-ptr
		push	ZL
		push	oloop			; Save counters
		push	iloop				

		clr		zero			; Maintain zero semantics

		; Set Y to beginning address of B
		ldi		YL, low(addrB)	; Load low byte
		ldi		YH, high(addrB)	; Load high byte

		; Set Z to begginning address of resulting Product
		ldi		ZL, low(LAddrP)	; Load low byte
		ldi		ZH, high(LAddrP); Load high byte

		; Begin outer for loop
		ldi		oloop, 2		; Load counter
MUL16_OLOOP:
		; Set X to beginning address of A
		ldi		XL, low(addrA)	; Load low byte
		ldi		XH, high(addrA)	; Load high byte

		; Begin inner for loop
		ldi		iloop, 2		; Load counter
MUL16_ILOOP:
		ld		A, X+			; Get byte of A operand
		ld		B, Y			; Get byte of B operand
		mul		A,B				; Multiply A and B
		ld		A, Z+			; Get a result byte from memory
		ld		B, Z+			; Get the next result byte from memory
		add		rlo, A			; rlo <= rlo + A
		adc		rhi, B			; rhi <= rhi + B + carry
		ld		A, Z			; Get a third byte from the result
		adc		A, zero			; Add carry to A
		st		Z, A			; Store third byte to memory
		st		-Z, rhi			; Store second byte to memory
		st		-Z, rlo			; Store first byte to memory
		adiw	ZH:ZL, 1		; Z <= Z + 1			
		dec		iloop			; Decrement counter
		brne	MUL16_ILOOP		; Loop if iLoop != 0
		; End inner for loop

		sbiw	ZH:ZL, 1		; Z <= Z - 1
		adiw	YH:YL, 1		; Y <= Y + 1
		dec		oloop			; Decrement counter
		brne	MUL16_OLOOP		; Loop if oLoop != 0
		; End outer for loop
		 		
		pop		iloop			; Restore all registers in reverves order
		pop		oloop
		pop		ZL				
		pop		ZH
		pop		YL
		pop		YH
		pop		XL
		pop		XH
		pop		zero
		pop		rlo
		pop		rhi
		pop		B
		pop		A
		ret						; End a function with RET

;-----------------------------------------------------------
; Func: Template function header
; Desc: Cut and paste this and fill in the info at the 
;		beginning of your functions
;-----------------------------------------------------------
FUNC:							; Begin a function with a label
		; Save variable by pushing them to the stack

		; Execute the function here
		
		; Restore variable by popping them from the stack in reverse order
		ret						; End a function with RET


;***********************************************************
;*	Stored Program Data
;***********************************************************

; Enter any stored data you might need here

; ADD16 operands
ADD16_PM1:
	.DW 0xFCBA
ADD16_PM2:
	.DW 0xFFFF
; SUB16 operands 0xFCB9 and 0xE420
SUB16_PM1:
	.DW 0xFCB9
SUB16_PM2:
	.DW 0xE420
; MUL24 operands

MUL24_PM1:
	.DW 0xFFFFFF
MUL24_PM2:
	.DW 0xFFFFFF

; Compoud operands
OperandD:
	.DW	0xFCBA				; test value for operand D
OperandE:
	.DW	0x2019				; test value for operand E
OperandF:
	.DW	0x21BB				; test value for operand F

;***********************************************************
;*	Data Memory Allocation
;***********************************************************

.dseg
.org	$0100				; data memory allocation for MUL16 example
addrA:	.byte 2
addrB:	.byte 2
LAddrP:	.byte 4

; Below is an example of data memory allocation for ADD16.
; Consider using something similar for SUB16 and MUL24.

.org	$0110				; data memory allocation for operands
ADD16_OP1:
		.byte 2				; allocate two bytes for first operand of ADD16
ADD16_OP2:
		.byte 2				; allocate two bytes for second operand of ADD16

.org	$0120				; data memory allocation for results
ADD16_Result:
		.byte 3				; allocate three bytes for ADD16 result


;Allocation for SUB 
.org $0130					;subtranction will start at $0130 in data memory
SUB16_OP1:
	.byte 2					; allocate two bytes for the first operand of SUB
SUB16_OP2:
	.byte 2					; allocate another two bytes for the second operand of SUB

.org $0140					; the result of the SUB operation will be stored at data memory location $0140
SUB16_Result:
	.byte 2					; allocate two bytes in data memory (lab specified no negative numbers, therefore only 2 bytes needed)


;Allocation for MUL24
.org $0150					; start of first MUL24 operand
MUL24_OP1:
	.byte 3
.org $0160
MUL24_OP2:
	.byte 3

.org $0170					; start of result location
MUL24_Result:
	.byte 6

.org $0180					;for storage
	.byte 12


.org $01A0					; operands in compound function
	.byte 6

.org $01B0
	.byte 6					; result of compound function

;***********************************************************
;*	Additional Program Includes
;***********************************************************
; There are no additional file includes for this program
