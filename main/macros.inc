;----------------------------------
; CONSTANTS
;----------------------------------

;----------------------------------
; LCD RELATED MACROS
;----------------------------------
;***************************************************
; P0_P1call: call to page 1 subroutine from page 0 *
;***************************************************
P0_P1call macro label
	bsf		PCLATH, 3				; Select Page 1
	call	label
	bcf		PCLATH, 3				;reSelect Page 0
	endm
	
;******************************
; LCD_DELAY: Delay for ~102us *
;******************************
LCD_DELAY macro
		local	Loop
		movlw	0xFF				;move the required value of cycles to W
		movwf	lcd_d1				;move the required value from W to a register
Loop
		decfsz	lcd_d1,f			;decrement the value and skip the next inst. when 0
		goto	Loop					;return to the prev. inst. (decrement), skipped if 0
		endm

;************************************************************
; Display: Displays a given Message (as a table) to the LCD *
;															*
; Parameters:												*
; -----------												*
;	Message --> the address of the Table to display			*
;	Start --> the start position in HEX as per LCD mapping	* 
;************************************************************
Display macro Message, Start
		local	loop_				;declare local labels
		local	end_
		clrf	Table_Counter		;table_counter will contain the char to display
		
		;Set the Start Position
		movlw	H'80'				;the instruction to specify position
		addlw	Start				;modify the instruction with the correct start position
		P0_P1call	WR_INS				;Write the instruction to the LCD

		clrw
loop_
		movf	Table_Counter,W		;table counter contains the current char (n) that is next to display
		P0_P1call	Message				;this returns the (n)th character into W 
		xorlw	B'00000000'			;This would yield 0 if the W contains 0 (end of Message)
		btfsc	STATUS,Z			;Z bit is cleared (0) if W wasn't 0 (not end of Message) and will skip
		goto	end_				;Won't be skipped only if Z was set (1), i.e. end of Message is reached
		P0_P1call	WR_DATA				;Write the data to the LCD
		incf	Table_Counter,f		;increment the table_counter to go to the next word
		goto	loop_
end_
		endm
		
;************************************************
; LeftShift: Shifts Display n times to the left *
;************************************************
LeftShift macro n
	local	repeat_
	movlw	n
	movwf	Shift_counter
repeat_
	movlw	B'00011000'
	P0_P1call	WR_INS
	P0_P1call	HalfS
	decfsz	Shift_counter
	goto	repeat_
	endm	
;----------------------------------
;OTHER MACROS
;----------------------------------

;**********************************************************
; PCinc: properly increments PCL taking int accunt PCLATH *
;**********************************************************
PCinc macro	TableEntries
	movwf	Temp
	movlw	HIGH TableEntries
	movwf	PCLATH
	movf	Temp,W
	addlw	LOW TableEntries
	btfsc	STATUS,C
	incf	PCLATH,f
	movwf	PCL
	endm

;***********************************
; Delay: delay for (n)*0.5 seconds *
;***********************************
Delay macro	n
	local 	DelayLoop
	banksel	D_count
	movlw	n
	movwf	D_count
DelayLoop
	P0_P1call	HalfS
	decfsz	D_count,f
	goto 	DelayLoop
	endm
	
;*********************************************
; SolenoidDelay: delay for 0.005*(n) seconds *
;*********************************************
SolenoidDelay macro n
	local	DelayLoop
	banksel	D_count
	movlw	n
	movwf	D_count
DelayLoop
	call	StepperDelay
	decfsz	D_count, f
	goto	DelayLoop
	endm
	

;***********************************************
; Stepf: Move the stepper forward for (n) steps *
;***********************************************
Stepf macro	n
	local	Loop
	local	D_LOOP
	banksel	PORTC
	movlw	n
	movwf	tmp_count
Loop
	movlw	B'00100000'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay
	movlw	B'00000001'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay
	movlw	B'00000010'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay
	movlw	B'00000100'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay
	decfsz	tmp_count, f
	goto	Loop
	endm
	
;*************************************************
; Stepb: Move the stepper backward for (n) steps *
;*************************************************
Stepb macro	n
	banksel	PORTC
	movlw	n
	movwf	tmp_count
	local	Loop
Loop
	movlw	B'00100000'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay 
	movlw	B'00000100'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay
	movlw	B'00000010'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay
	movlw	B'00000001'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	call	StepperDelay
	decfsz	tmp_count, f
	goto	Loop
	endm
	
;*********************************************
; Dispense_Brown: dispenses one brown dowel	 *
;*********************************************
Dispense_Brown macro
	local Wait_For_Dowel
	local Dispense_loop
	local Delay_0
	local end_macro
	banksel	NbDowels_B
Wait_For_Dowel
	movf	NbDowels_B, W
	xorlw	d'0'				; Check if Nb of dowels is 0
	btfsc	STATUS, Z			; Get out of the loop only if at least one dowel is ready
	goto	Wait_For_Dowel
	
Dispense_loop	
	bsf		PORTA, 1			; Turn On the solenoid
	SolenoidDelay  d'30'		; Delay for sometime
	bcf		PORTA, 1			; Turn Off the Solenoid
	; Polling Delay Loop
	movlw	0x88
	movwf	d1
	movlw	0xBA
	movwf	d2
	movlw	0x03
	movwf	d3
Delay_0
	btfsc   PORTB, 2
	goto	end_macro
	;*****TEMPORARY
	btfsc	PORTC, 6
	goto	end_macro
	;TEMPORARY*****
	decfsz	d1, f
	goto	$+2
	decfsz	d2, f
	goto	$+2
	decfsz	d3, f
	goto	Delay_0
	goto	Dispense_loop		; Loop until a dowel is dispensed
end_macro
	decf	NbDowels_B, f
	endm

;*********************************************
; Disepnse_White: dispenses one white dowel	 *
;*********************************************
Dispense_White macro
	local Wait_For_Dowel
	local Dispense_loop
	local Delay_0
	local end_macro
	banksel	NbDowels_W
Wait_For_Dowel
	movf	NbDowels_W, W
	xorlw	d'0'				; Check if Nb of dowels is 0
	btfsc	STATUS, Z			; Get out of the loop only if at least one dowel is ready
	goto	Wait_For_Dowel
	
Dispense_loop	
	bsf		PORTA, 2			; Turn On the solenoid
	SolenoidDelay  d'30'		; Delay for sometime
	bcf		PORTA, 2			; Turn Off the Solenoid
	; Polling Delay Loop
	movlw	0x88
	movwf	d1
	movlw	0xBA
	movwf	d2
	movlw	0x03
	movwf	d3
Delay_0
	btfsc   PORTB, 2
	goto	end_macro
	;*****TEMPORARY
	btfsc	PORTC, 6
	goto	end_macro
	;TEMPORARY*****
	decfsz	d1, f
	goto	$+2
	decfsz	d2, f
	goto	$+2
	decfsz	d3, f
	goto	Delay_0
	goto	Dispense_loop		; Loop until a dowel is dispensed
end_macro
	decf	NbDowels_W, f
	endm
	
;**********************************************************************
; BCD: takes in a register and changes it into BCD in BCD_H and BCD_L *
; NOTE ----> Only deals with number up to 19                          *
;**********************************************************************
BCD	macro f
	local	less_than_9

	movf	f, w
	sublw	d'10'
	movwf	tmp
	btfsc	tmp, 7				; if the 7th bit is 1 then it's negative
	goto	less_than_9 		; if it is then the number was less than 9
	; if it's clear and doesn't skip (number is positive)
	movwf	BCD_L				; move the result of the subtraction into BCD_L
	movlw	d'1'
	movf	BCD_H				; set the high byte to 1
less_than_9
	; If it's less than 9 then just move the number into BCD_L
	movf	f, w	
	movwf	BCD_L				
	clrf	BCD_H				; make sure the high byte is 0
	endm