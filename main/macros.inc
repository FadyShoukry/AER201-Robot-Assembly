;----------------------------------
; CONSTANTS
;----------------------------------

;----------------------------------
; LCD RELATED MACROS
;----------------------------------
;********************************************
; Scall: do page selection and call (label) *
;********************************************
Scall macro label
	bsf		PCLATH,3				; Select Page 1
	call	label					; call the subroutine in Page 1
	bcf		PCLATH, 3				; reselect Page 0 when you return
	endm
	
;******************************
; LCD_DELAY: Delay for ~102us *
;******************************
LCD_DELAY macro
		movlw	0xFF				;move the required value of cycles to W
		movwf	lcd_d1				;move the required value from W to a register
		decfsz	lcd_d1,f			;decrement the value and skip the next inst. when 0
		goto	$-1					;return to the prev. inst. (decrement), skipped if 0
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
		Scall	WR_INS				;Write the instruction to the LCD

		clrw
loop_
		movf	Table_Counter,W		;table counter contains the current char (n) that is next to display
		Scall	Message				;this returns the (n)th character into W 
		bcf		PCLATH, 3			; reselect page 0
		xorlw	B'00000000'			;This would yield 0 if the W contains 0 (end of Message)
		btfsc	STATUS,Z			;Z bit is cleared (0) if W wasn't 0 (not end of Message) and will skip
		goto	end_				;Won't be skipped only if Z was set (1), i.e. end of Message is reached
		Scall	WR_DATA				;Write the data to the LCD
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
	Scall	WR_INS
	Scall	HalfS
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
	movlw	n
	movwf	D_count
	Scall	HalfS
	decfsz	D_count,f
	goto 	$-2
	endm

;***********************************************
; Stepf: Move the stepper forward for (n) steps *
;***********************************************
Stepb macro	n
	banksel	PORTC
	movlw	n
	movwf	tmp_count
	local	Loop
Loop
	movlw	B'00001000'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay 
	movlw	B'00000001'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay
	movlw	B'00000010'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay
	movlw	B'00000100'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay
	decfsz	tmp_count, f
	goto	Loop
	endm
	
;*************************************************
; Stepb: Move the stepper backward for (n) steps *
;*************************************************
Stepf macro	n
	banksel	PORTC
	movlw	n
	movwf	tmp_count
	local	Loop
Loop
	movlw	B'00001000'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay 
	movlw	B'00000100'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay
	movlw	B'00000010'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay
	movlw	B'00000001'			; The rotating pattern to turn Stepper ON
	movwf	PORTC
	Scall	StepperDelay
	decfsz	tmp_count, f
	goto	Loop
	endm
	
;*********************************************
; Dispense_Brown: dispenses one brown dowel	 *
;*********************************************
Dispense_Brown macro
	local Wait_For_Dowel
	local Dispense_loop
	banksel	NbDowels_B
Wait_For_Dowel
	movf	NbDowels_B, W
	xorlw	d'0'				; Check if Nb of dowels is 0
	btfsc	STATUS, Z			; Get out of the loop only if at least one dowel is ready
	goto	Wait_For_Dowel
	
Dispense_loop	
	bsf		PORTA, 1			; Turn On the solenoid
	Delay	d'4'				; Delay for sometime
	bcf		PORTA, 1			; Turn Off the Solenoid
	Delay   d'4'
	decf	NbDowels_B, f
	btfss	Sensor_state, 3
	goto	Dispense_loop		; Loop until a dowel is dispensed
	endm

;*********************************************
; Disepnse_White: dispenses one white dowel	 *
;*********************************************
Dispense_White macro
	local Wait_For_Dowel
	local Dispense_loop
	banksel	NbDowels_W
Wait_For_Dowel
	movf	NbDowels_W, W
	xorlw	d'0'				; Check if Nb of dowels is 0
	btfsc	STATUS, Z			; Get out of the loop only if at least one dowel is ready
	goto	Wait_For_Dowel
	
Dispense_loop	
	bsf		PORTA, 2			; Turn On the solenoid
	Delay	d'4'				; Delay for sometime
	bcf		PORTA, 2			; Turn Off the Solenoid
	Delay	d'4'
	btfss	Sensor_state, 2
	goto	Dispense_loop		; Loop until a dowel is dispensed
	decf	NbDowels_W, f
	endm