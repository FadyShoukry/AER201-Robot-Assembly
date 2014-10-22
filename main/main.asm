	errorlevel	-306
;====================================== 
; MICROCONTROLLER DECLARATION
;======================================
	list p=16f877
	#include <p16f877.inc>
	
;======================================
; CONFIGURATION
;======================================
	__CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF


;======================================
; INCLUDES and MACROS
;======================================
	#include <macros.inc>
	
;======================================
; VARIABLES and CONSTANTS
;======================================
	;I/O LCD PINS TEXT SUBSTITUTES
	#define RS 		PORTD,2
	#define E 		PORTD,3
	#define DAV 	PORTB,1
	
	;Keypad mapping values
	#define Key0	H'0D'
	#define Key1	H'00'
	#define Key2	H'01'
	#define Key3	H'02'
	#define Key4	H'04'
	#define Key5	H'05'
	#define Key6	H'06'
	#define KeyA	H'03'
	#define KeyB	H'07'
	#define KeyC	H'0B'
		
	;VARIABLES
	cblock 0x20
		;Counters for the 0.5 sec. delay
		COUNTH
		COUNTM
		COUNTL
		
		;Counter to traverse the table for display
		Table_Counter
		
		;Counters for the LCD delay loops
		lcd_d1
		lcd_d2
		
		;Register to hold the data
		dat
		
		;Register to store the command
		com
		
		;Counter for the Delay macro
		D_count
		
		;A temporary register for general storage
		tmp
		
		;A counter for the shift macro
		Shift_counter
		
		;Variable used in the PCLATH increment macro for Tables
		Temp
		
		;Variable to store the number of boxes
		num_boxes
		
		;Variable for a box counter in Pattern Loop
		box_count
		
		; Variable to store the Pattern number for validation
		pattern
		
		; Operation details Variables
		patternNb:3
		
		; Dowel Counter Registers
		NbDowels_W
		NbDowels_B
		
		; For the number of dowels display we need 2 register for high and low bytes (BCD)
		BCD_L
		BCD_H
		
		; Operation time Keeping registers
		Cycles
		Seconds
		Minutes
		
		; Temporary registers to save W and STATUS
		STATUS_TMP			; Will require 2 levels in case of nested interrupts
		W_TMP
		
		; this variable determines the state of the of the sensor to avoid multiple counting
		; bit0 --> White_Count    bit1--> Brown_Count		
		Sensor_state 		

		; Stepper On/Off Indicator --> Off if 0 On otherwise
		StepperInd
		
		; A temporary variable only used in ISRS
		tmp_ISR
		
		; a temperoary variable used for counting macros only
		tmp_count
		
		; A counter for DC motor cycles
		DC_Cycles
		DC_Cycles_H
		
		; Delay loop registers
		d1
		d2
		d3
		
		; A register to store a custom pattern
		CustomPattern
	endc

;+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
; 				PAGE 0				_+
;+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
;======================================
; PROGRAM VECTOR DECLARATION
;======================================
	;Reset Vector
	ORG		0x0000
	goto	INIT	;jump to the initialization program
	
	; Interrupt Vector
	ORG		0x0004
	nop
;======================================
; Interrupt sevice routine
;======================================

ISR
	; Save STATUS and W registers
	movwf	W_TMP
	movf	STATUS, W
	movwf	STATUS_TMP
	
	;**Check for the cause of the interrupt**
	; Check if it's the TIMER
	banksel	PIR1
	btfss	PIR1, 0					; Check if the Timer1 Flag is set
	goto	Check_RD0				; if not Check PORTB
	P0_P1call	TIMER_ISR				; if it is then P0_P1call the corresponding ISR
	bsf			PORTA, 4
Check_RD0
	; Check if it's the White Dowel Counter (RB7)
	banksel	PORTD
	btfsc	PORTD, 0
	goto 	RD0High
	;*****TEMPORARY
	btfsc	PORTC, 3
	goto	RD0High
	;TEMPORARY*****
	bcf		Sensor_state, 0 		; Set the sensor bit for White = 0	
	goto	Check_RD1
	
RD0High
	btfsc	Sensor_state, 0 	     ; only if the state is 0 then increment
	goto	Check_RD1				 ; if it's still 1 then move to RB6
	; otherwise increment
	incf	NbDowels_W, f
	bsf		Sensor_state, 0 ; Set the sensor state to 1 now to avoid multiple counts
	
Check_RD1
	; Check if it's the Brown Dowel Counter (RB6)
	btfsc	PORTD, 1
	goto 	RD1High
	;*****TEMPORARY
	btfsc	PORTC, 4
	goto	RD1High
	;TEMPORARY*****
	bcf		Sensor_state, 1 ; Set hte sensor bit for Brown = 0	
	goto	END_ISR

RD1High
	btfsc	Sensor_state, 1 ; only if the state is 0 then increment
	goto	END_ISR				 ; if it's still 1 then move to END
	; otherwise increment the Brown Dowels count
	incf	NbDowels_B, f
	bsf		Sensor_state, 1 ; Set the sensor state to 1 now to avoid multiple counts
	
END_ISR
	; Clear the RBIF PORTB Flag just in case
	bcf		INTCON, RBIF
	; restore the saved Status and W registers
	movf	STATUS_TMP, W
	movwf	STATUS
	movf	W_TMP, W
	retfie
	
;=====================================
; INITIALIZATION
;=====================================
INIT
	;**Initialize I/O Port Registers**
	clrf      	INTCON         	; No interrupts needed in this code

    bsf       	STATUS,RP0     	; select bank 1
    clrf      	TRISA          	; All port A is output
    movlw     	b'11110110'    	; Set RB<7:4> and RB1 and RB2 to input
    movwf     	TRISB
	movlw		b'01011000'		; All output except for the I2C pins RC3 and RC4
    movwf      	TRISC			; All output
    movlw		b'00000011'
    movwf      	TRISD			; Port D is all output except RD0 and RD1
	clrf		TRISE			; Ensure not in slave mode and all outputs
	movlw		B'00001110'
	movwf		ADCON1
	
    bcf       	STATUS,RP0     	; select bank 0
    clrf      	PORTA
    clrf      	PORTB
    clrf      	PORTC
    clrf      	PORTD
    
    ;**Turn off the Hopper DC motors**
	;bsf			PORTE, 1		; Set the motor OFF
   
	;**Initialize LCD**
	P0_P1call 		InitLCD
	
	
;======================================
; MAIN CODE
;======================================
	;**Initial Display**
	Display 	WelcomeMsg_1, H'03'
	Display 	WelcomeMsg_2, H'41'
	
	Delay		d'6'				;3 seconds delay
	P0_P1call		ClrLCD
	
;	Display		InitializingMsg, H'02'
;	Display		PleaseWait,	H'41'
;	
;	Delay		d'6'
	
NEW_OPERATION
	P0_P1call	ClrLCD
	Display		Prompt0, H'00'
	Display		aNewOperation, H'40'
	
	Delay		d'3'
	
Poll_2
	P0_P1call	PollKeys		; Poll for the A Key
	xorlw		KeyA			
	btfss		STATUS,Z		; If pressed (Z = 1) skip and proceed
	goto		Poll_2			; Else, continue polling
	
	;**Start New Operation**
;	;**Input instructions**
;	P0_P1call		OneLineMode
;	P0_P1call		ClrLCD
;	Display		InputInstrcs, H'10'
;	LeftShift	d'50'
;	
;	Delay		d'3'
	
	;**Number of boxes Prompt**
Box_prompt
	P0_P1call		ClrLCD
	P0_P1call		TwoLineMode
	Display		BoxMsg_1, H'00'
	Display		BoxMsg_2, H'40'
	P0_P1call		DisplayCursor
	
	;**Poll for an entry and display it**
	P0_P1call		PollKeys
	movwf		num_boxes		; store the input (will be changes to a number later)
	P0_P1call		HextoChar		; Find the corresponding character
	P0_P1call		WR_DATA			; Display the character
	P0_P1call		HideCursor		; Hide the cursor
	
	;**Poll for validation or cancellation**
Poll_3
	P0_P1call	PollKeys
	movwf		tmp
	xorlw		KeyA			; Check if A is pressed
	btfsc		STATUS,Z		; If it's not A then skip and check if B
	goto		Box_Validity	; If it's, go to check the input validity
	movf		tmp,W			; restore the input from tmp
	xorlw		KeyB			; Check if it's B
	btfss		STATUS,Z		
	goto		Poll_3			; If not B go back to polling
	goto		Box_prompt		; If it's B then cancel and go back to prompt
	
	;**Input Validity Check (1<#<4)**
Box_Validity
	movf		num_boxes,W		; move the stored input to W		
	xorlw		Key1			; Check if it's 1
	btfss		STATUS,Z		; if It's 1 skip further checks
	goto		Check_2			; if it's not check if it's 2
	movlw		d'1'			
	movwf		num_boxes		; if it's 1 then store 1 in the register
	goto		Pattern			; proceed to Pattern Section
			
Check_2
	movf		num_boxes,W		; move the stored input to W		
	xorlw		Key2			; Check if it's 2
	btfss		STATUS,Z		; if It's 2 skip further checks
	goto		Check_3			; if it's not check if it's 3
	movlw		d'2'			
	movwf		num_boxes		; if it's 2 then store 2 in the register
	goto		Pattern			; proceed to Pattern Section
	
Check_3
	movf		num_boxes,W		; move the stored input to W		
	xorlw		Key3			; Check if it's 3
	btfss		STATUS,Z		; if It's 1 skip further checks
	goto		Box_Invalid		; if it's not check if it's 4
	movlw		d'3'			
	movwf		num_boxes		; if it's 3 then store 3 in the register
	goto		Pattern			; proceed to Pattern section	
	
Box_Invalid
	P0_P1call		ClrLCD
	Display		InvalidInput, H'01'
	
	Delay		d'3'
	
;	P0_P1call		ClrLCD
;	P0_P1call		OneLineMode
;	Display		BoxInvalidMsg, H'10'
;	LeftShift	d'50'
	goto		Box_prompt
	
Pattern	
;	;**Pattern Viewer Instructions**
;	P0_P1call		ClrLCD
;	P0_P1call		OneLineMode
;	P0_P1call		DisplayOFF
;	Display		PatternViewerInst, H'08'
;	P0_P1call		DisplayON
;	LeftShift	d'70'
;	
;	;**Prompt for input for either a list of Patterns or proceeding**
;	P0_P1call		PollKeys
;	xorlw		KeyC			; Check if it's C
;	btfss		STATUS,Z		; If it's C then skip the jump and display patterns
;	goto		Pattern_Prompt	; If it's not C then jump to the Pattern prompt
	
;	;**Available Patterns Display**
;	P0_P1call		ClrLCD
;	P0_P1call		TwoLineMode
;	Display		Pattern1, H'00'	; First screen		
;	Display		Pattern2, H'40'
;	
;	Delay		d'6'
;	
;	P0_P1call		ClrLCD
;	Display		Pattern3, H'00'	; Second screen		
;	Display		Pattern4, H'40'
;	
;	Delay		d'6'
;	
;	P0_P1call		ClrLCD
;	Display		Pattern5, H'00'	; Third screen		
;	Display		Pattern6, H'40'
;	
;	Delay		d'6'
;	
;	goto		Pattern			; Return to the Pattern viewer instructions in case the user wants another look
	
	;**Prompt for Pattern Number**
	; This will loop for the stored number of boxes
Pattern_Prompt	
	clrf		box_count		; initialize the box_count variable to 0
Pattern_Prompt_Loop
	P0_P1call		ClrLCD
	P0_P1call		TwoLineMode
	Display		PatternMsg_1, H'00'
	Display		PatternMsg_2, H'40'
	
	; Display the box number and a " : "
	movf		box_count, W		; move the box count to W
	P0_P1call		Numbers			; Display the number of box_count
	P0_P1call		WR_DATA
	movlw		":"				; Display the " : "
	P0_P1call		WR_DATA	
	P0_P1call		DisplayCursor	; Display the Cursor
	
	;**Poll for an entry and display it**
	P0_P1call		PollKeys
	movwf		pattern			; store the input (will be changes to a number later)
	P0_P1call		HextoChar		; Find the corresponding character
	P0_P1call		WR_DATA			; Display the character
	P0_P1call		HideCursor		; Hide the cursor
	
	;**Poll for validation or cancellation**
Poll_4
	P0_P1call		PollKeys
	movwf		tmp
	xorlw		KeyA			; Check if A is pressed
	btfsc		STATUS,Z		; If it's not A then skip and check if B
	goto		Pattern_Validity; If it's, go to check the input validity
	movf		tmp,W			; restore the input from tmp
	xorlw		KeyB			; Check if it's B
	btfss		STATUS,Z		
	goto		Poll_4			; If not B go back to polling
	goto		Pattern_Prompt	; If it's B then cancel and go back to prompt
	
Pattern_Validity
	movf		pattern,W		; move the stored input to W		
	xorlw		Key1			; Check if it's 1
	btfss		STATUS,Z		; if It's 1 skip further checks
	goto		P_Check_2			; if it's not check if it's 2
	movlw		d'1'			
	movwf		pattern			; if it's 1 then store 1 in the register
	goto		while_cond		; Check Repeat condition
			
P_Check_2
	movf		pattern,W		; move the stored input to W		
	xorlw		Key2			; Check if it's 2
	btfss		STATUS,Z		; if It's 2 skip further checks
	goto		P_Check_3			; if it's not check if it's 3
	movlw		d'2'			
	movwf		pattern			; if it's 2 then store 2 in the register
	goto		while_cond		; Check Repeat condition
	
P_Check_3
	movf		pattern,W		; move the stored input to W		
	xorlw		Key3			; Check if it's 3
	btfss		STATUS,Z		; if It's 1 skip further checks
	goto		P_Check_4			; if it's not check if it's 4
	movlw		d'3'			
	movwf		pattern			; if it's 3 then store 3 in the register
	goto		while_cond		; Check Repeat condition
	
P_Check_4
	movf		pattern,W		; move the stored input to W		
	xorlw		Key4			; Check if it's 4
	btfss		STATUS,Z		; if It's 4 skip further checks
	goto		P_Check_5			; if it's not then the input is invalid
	movlw		d'4'			
	movwf		pattern			; if it's 4 then store 4 in the register
	goto		while_cond		; Check Repeat condition
	
P_Check_5
	movf		pattern,W		; move the stored input to W		
	xorlw		Key5			; Check if it's 5
	btfss		STATUS,Z		; if It's 5 skip further checks
	goto		P_Check_6		; if it's not then the input is invalid
	movlw		d'5'			
	movwf		pattern			; if it's 5 then store 5 in the register
	goto		while_cond		; Check Repeat condition
	
P_Check_6
	movf		pattern,W		; move the stored input to W		
	xorlw		Key6			; Check if it's 6
	btfss		STATUS,Z		; if It's 6 skip further checks
	goto		Pattern_Invalid	; if it's not then the input is invalid
	movlw		d'6'			
	movwf		pattern			; if it's 6 then store 6 in the register
	goto		while_cond		; Check Repeat condition

;P_Check_B						; for a custom pattern
;	movf		patern, W
;	xorlw		KeyB			; Check if it's B
;	btfss		STATUS, Z
;	goto		Pattern_Invalid ; if it's not 6 then the pattern is invalid
;	; If it is B then prompt for a custom pattern
;	P0_P1call	ClrLCD
;	Display		CustomPatternPrompt, H'00'
;	Delay 		d'3'
;	
;	P0_P1call	ClrLCD
;	Display		CustomPatternInst1, H'00'
;	Displa
Pattern_Invalid
	P0_P1call		ClrLCD
	Display		InvalidInput, H'01'
	
	Delay		d'3'
	
;	P0_P1call		ClrLCD
;	P0_P1call		OneLineMode
;	Display		PatternInvalidMsg, H'10'
;	LeftShift	d'50'
	goto		Pattern_Prompt_Loop

while_cond
	movlw		patternNb		;Initialize array pointer
	addwf		box_count, w	; Get to the current index
	movwf		FSR				; Set the correct address for indirect addressing
	movf		pattern, W		; move the recorded pattern number in W
	movwf		INDF			; move the pattern number into the array
	incf		box_count
	decfsz		num_boxes
	goto		Pattern_Prompt_Loop
;	clrf		box_count		; Clear the box_count variable to 0 for the next time!
	
	;**Start Process**
	P0_P1call		ClrLCD
	Display		EndMsg_1, H'02'
	Display		EndMsg_2, H'41'

	
	;**Setup Interrupts**
	banksel		PIR1
	clrf		PIR1			; Ensure Interrupt Flags are cleared
	bsf			INTCON, PEIE	; Enable Peripheral Interrupts
	banksel		PIE1
	bsf			PIE1, TMR1IE	; Enable the Timer1 Peripheral Interrupts
	bsf			INTCON, GIE		; Enable general interrupts
	
	;**Initiliaze time keeping registers**
	banksel		Cycles
	clrf		Cycles
	clrf		DC_Cycles
	clrf		DC_Cycles_H
	clrf		Seconds
	clrf		Minutes
	
	;** Intialize counting registers**
	clrf		Sensor_state
	clrf		NbDowels_W
	clrf		NbDowels_B
	
	;**Setup the timer**
	banksel		TMR1H
	clrf		TMR1L			; Ensure the Timer starts at 0
	clrf		TMR1H
	movlw		B'00000001'
	movwf		T1CON			; Start the Timer
	
	;**Start the Hopper DC motors**
	banksel		PORTE
	bcf			PORTE, 0
	bsf			PORTE, 1		; Set the motor ON
	bsf			PORTE, 2
	;**Initiate Moving the BMA**
	clrf		num_boxes		; Will be used as a counter for the boxes

Process
	; Move BMA forward
	Stepf	d'100'
	Stepf	d'170'
	;**Grab Box**
	bsf		PORTA, 0			; Power BMA solenoid --> release the solenoid
	Stepf	d'35'				; Step 5 steps
	bcf		PORTA, 0			; Turn off the BMA solenoid --> hold the solenoid
	
	;**Move the Stepper in the opposite direction**
	Stepb	d'105'

	;**Dispensing code**
	movlw		d'6'
	movwf		tmp				; Will be used as a counter for the individual dispense loop
	movlw		patternNb		; Initialize array pointer
	addwf		num_boxes, w	; Get to the current index
	movwf		FSR				; Set the correct address for indirect addressing
	decf		INDF, w			; move the pattern number - 1 in the array into W (counting from 0 instead of 1)
	P0_P1call	Patterns
	movwf		pattern			; use it to test the bits
Dispensing_Loop
	btfss		pattern, 0		; test bit 0
	goto		White			; if the bit is not 1 then dispense white
	Dispense_Brown				; if it is 1 then dispense brown
	Delay		d'6'
	decfsz		tmp				
	goto		Next
	goto		Done_Dispensing
White
	Dispense_White
	Delay		d'4'
	decfsz		tmp
	goto		Next	; continue looping for the 6 positions
	goto		Done_Dispensing
Next
	Stepb		d'30'			; Move the BMA to next position
	rrf			pattern			; move the next bit to position 0
	goto		Dispensing_Loop
	
	;**Close and send the box out
Done_Dispensing
	Stepb		d'20'
	Stepf		d'2'
	banksel		PORTA
	bsf			PORTA, 0		; Turn On the BMA solenoid
	SolenoidDelay d'20'
	bcf			PORTA, 0
	SolenoidDelay d'40'
	bsf			PORTA, 0		; Turn On the BMA solenoid
	SolenoidDelay d'20'
	bcf			PORTA, 0
	SolenoidDelay d'40'
	bsf			PORTA, 0		; Turn On the BMA solenoid
	SolenoidDelay d'20'
	bcf			PORTA, 0
	SolenoidDelay d'40'
	bsf			PORTA, 0
	; Retract the box snapping solenoid
	bsf			PORTA, 5
	; Move BMA back
	Stepb		d'45'			; move all the way to the back
	bcf			PORTA, 0		; Turn Off the BMA solenoid
	; Snapping spasm algorithm
	Delay		d'2'			; wait for a second
	movlw		d'5'			; counter to repeat 5 times
	movwf		tmp
	; Spasming loop
Spasm_Loop
	bcf			PORTA, 5		; Hit
	Delay		d'1'			; Wait for a second
	bsf			PORTA, 5		; Retract
	SolenoidDelay	d'40'
	decfsz		tmp
	goto		Spasm_Loop		; Repeat for 5 times
	Delay		d'4'
	bsf			PORTA, 5
	
	;**Finish process and check if done**
	incf		num_boxes		; increment the number of boxes dispensed
	movf		num_boxes, W
	xorwf		box_count, W	; checking if box_count and num_boxes are equal
	btfss		STATUS, Z
	goto		Process			; if not euqal then loop back and  dispense the next box
	; Otherwise, if equal, terminate.
	
	; Turn DC motor Off
	bcf			PORTE, 2
	clrf		PORTE
	; Turn the Stepper Off
	bcf			PORTC, 0
		
	;**Completion**
	; Disable Timer
	banksel		T1CON
	clrf		T1CON			; Disable Timer
	P0_P1call		ClrLCD
	Display		Operation, H'03'
	Display		Completed, H'43'
	
	Delay		d'10'
	
	; **Display Operation Info**
	P0_P1call		ClrLCD
	Display		Operation, H'01'
	Display		Info, H'0B'
	; Display the dispensed patterns
	Display		Patterns_Word, H'40'
	; Loop to display all patterns in the array
	clrf		tmp				; used a counter for indexing
Loop
	movlw		patternNb		; initialize the array pointer
	addwf		tmp, W			; get the current index
	movwf		FSR				; move the current address into FSR
	decf		INDF, W			; move the value there into W
	P0_P1call		Numbers			; change into ASCII number value
	P0_P1call		WR_DATA			; write the Data
	; Check if done
	incf		tmp
	movf		tmp, W
	xorwf		num_boxes, w
	btfsc		STATUS, Z		; if equal move forward to display time
	goto		Display_Time
	; otherwise
	movlw		","
	P0_P1call		WR_DATA			; Write a comma
	goto		Loop
		
Display_Time
	Delay		d'10'
	P0_P1call		ClrLCD
	Display		Operation, H'01'
	Display 	Time, H'0B'
	; Display the minutes
	; Set the Start Position
	movlw		H'80'			;the instruction to specify position
	addlw		H'46'			;modify the instruction with the correct start position
	P0_P1call		WR_INS			;Write the instruction to the LCD
	movf		Minutes, W
	P0_P1call		Numbers_Zero	; Change to ASCII value of numbers counting 0
	P0_P1call		WR_DATA			; Display
	; Display ":"
	movlw		":"
	P0_P1call		WR_DATA
	; Display seconds
	swapf		Seconds, W
	andlw		H'0F'			; mask the unneeded bits
	P0_P1call		Numbers_Zero
	P0_P1call		WR_DATA
	movf		Seconds, W
	andlw		H'0F'
	P0_P1call		Numbers_Zero
	P0_P1call		WR_DATA
	Delay		d'5'
	
	;** Display the number of dowels**
	P0_P1call		ClrLCD
	Display		NumberofDowels, H'00'
	Delay		d'3'
	P0_P1call		ClrLCD
	Display		WhiteDowels, H'00'
	BCD			NbDowels_W
	movf		BCD_H, W
	P0_P1call		Numbers_Zero
	P0_P1call		WR_DATA
	movf		BCD_L, W
	P0_P1call		Numbers_Zero
	P0_P1call		WR_DATA
	Display		BrownDowels, H'40'
	BCD			NbDowels_B
	movf		BCD_H, W
	P0_P1call		Numbers_Zero
	P0_P1call		WR_DATA
	movf		BCD_L, W
	P0_P1call		Numbers_Zero
	P0_P1call		WR_DATA
	
goto 		$

   	
;*********************
; Stepper delay loop *
;*********************
StepperDelay
    movlw 	d'20'
   	movwf 	lcd_d2
	LCD_DELAY
	decfsz	lcd_d2, f
   	goto	$-2
   	return
   	
   	
;+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
; 				PAGE 1				_+
;+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+_+
ORG		0x800
;======================================
; TABLES
;======================================

NumberofDowels
	PCinc	NumberofDowelsEntries
NumberofDowelsEntries
	dt		"# of Dowels left", 0

Numbers
	PCinc	NumbersEntries
NumbersEntries
	dt		"123456789", 0
	
WhiteDowels
	PCinc	WhiteDowelsEntries
WhiteDowelsEntries
	dt		"White Dowels:", 0
	
BrownDowels
	PCinc	BrownDowelsEntries
BrownDowelsEntries
	dt		"Brown Dowels:", 0
	
WelcomeMsg_1	
	PCinc	WelcomeMsg_1Entries
WelcomeMsg_1Entries
	dt		"Welcome to", 0
		
WelcomeMsg_2
	PCinc	WelcomeMsg_2Entries
WelcomeMsg_2Entries
	dt		"WOODP(E/A)CKER", 0
		

PleaseWait
	PCinc	PleaseWaitEntries
PleaseWaitEntries
	dt		"Please Wait...",0

Operation
	PCinc	OperationEntries
OperationEntries
	dt		"Operation",0
	
Completed
	PCinc	CompletedEntries
CompletedEntries
	dt		"Completed",0

Prompt0
	PCinc	Prompt0Entries
Prompt0Entries
	dt		"Press A to Start",0
	
aNewOperation
	PCinc	aNewOperationEntries
aNewOperationEntries
	dt		"a New Operation",0
	
InputInstrcs
	PCinc	InputInstrcsEntries
InputInstrcsEntries
	dt		"For inputs, Press A to Validate or B to Cancel",0
	
BoxMsg_1
	PCinc	BoxMsg_1Entries
BoxMsg_1Entries
	dt		"Enter the # of",0
	
BoxMsg_2
	PCinc	BoxMsg_2Entries
BoxMsg_2Entries
	dt		"boxes to load:",0
	
HextoChar
	PCinc	HextoCharEntries
HextoCharEntries
	dt		"123A456B789C*0#D",0
	
InvalidInput
	PCinc	InvalidInputEntries
InvalidInputEntries
	dt		"Invalid Input!",0
	
BoxInvalidMsg
	PCinc	BoxInvalidMsgEntries
BoxInvalidMsgEntries
	dt		"# of boxes must be between 1 and 3 inclusive",0
	
PatternViewerInst
	PCinc	PatternViewerInstEntries
PatternViewerInstEntries
	dt		"For a list of available Patterns Press C. Press any other key to Proceed",0
	
Pattern1
	PCinc	Pattern1Entries
Pattern1Entries
	dt		"Pattern#1:WWWWWW",0
	
Pattern2
	PCinc	Pattern2Entries
Pattern2Entries
	dt		"Pattern#2:BBBBBB",0

Pattern3
	PCinc	Pattern3Entries
Pattern3Entries
	dt		"Pattern#3:WWWBBB",0
	
Pattern4
	PCinc	Pattern4Entries
Pattern4Entries
	dt		"Pattern#4:WWBBWW",0
	
Pattern5
	PCinc	Pattern5Entries
Pattern5Entries
	dt		"Pattern#5:BBWWBB",0
	
Pattern6
	PCinc	Pattern6Entries
Pattern6Entries
	dt		"Pattern#6:WBWBWB",0

PatternMsg_1
	PCinc	PatternMsg_1Entries
PatternMsg_1Entries
	dt		"Enter Pattern #",0
	
PatternMsg_2
	PCinc	PatternMsg_2Entries
PatternMsg_2Entries
	dt		"for Box ",0
	
PatternInvalidMsg
	PCinc	PatternInvalidMsgEntries
PatternInvalidMsgEntries
	dt		"Pattern # must be between 1 and 6 inclusive",0

EndMsg_1
	PCinc	EndMsg_1Entries
EndMsg_1Entries
	dt		"Now Sit Back",0
	
EndMsg_2
	PCinc	EndMsg_2Entries
EndMsg_2Entries
	dt		"Enjoy the Show",0
	
ReportMsg_1
	PCinc	ReportMsg_1Entries
ReportMsg_1Entries	
	dt		"# of Dowels left",0
	
Patterns
	PCinc	Patterns_Entries
Patterns_Entries
	retlw	B'00000000'
	retlw	B'11111111'
	retlw	B'00000111'
	retlw	B'00001100'
	retlw	B'00110011'
	retlw	B'00010101'
	
Info
	PCinc	Info_Entries
Info_Entries
	dt		"Info", 0
	
Patterns_Word
	PCinc	Patterns_Word_Entries
Patterns_Word_Entries
	dt		"Patterns:", 0
	
Time
	PCinc	Time_Entries
Time_Entries
	dt		"Time", 0
	
White_Word
	PCinc	White_Entries
White_Entries
	dt		"White", 0
	
Brown
	PCinc	Brown_Entries
Brown_Entries
	dt		"Brown", 0

Numbers_Zero
	PCinc		Numbers_Zero_Entries
Numbers_Zero_Entries
	dt		"0123456789", 0
	
;======================================
; SUBROUTINES
;======================================

;--------------------------------------
; ISR related subroutine
;--------------------------------------
TIMER_ISR
	banksel	PIR1
	bcf		PIR1, TMR1IF		; Clear the Timer Interrupt Flag (just in case)
	
	banksel	Cycles				; ensure we r in the correct bank (bank0)	
	incf	Cycles				; Increment the number of cycles
	incf	DC_Cycles			; Increment the number of DC cycles
	
	; Check if number of cycles reached 38 ~ 1s
	movf	Cycles, W
	xorlw	d'38'
	btfss	STATUS, Z
	goto	END_TIMER_ISR		; if it is not 38 then end
	; If it did reach 38 increment the  seconds count and reset the cycles count
	clrf	Cycles
	incf	Seconds

	; Check if the lower bits reached 9
	movf	Seconds, W
	andlw	H'0F'
	xorlw	d'09'
	btfss	STATUS, Z
	goto	END_TIMER_ISR
	
	; If it did reach 9 then reset the lower bits and increment upper bits
	movlw	H'F0'
	andwf	Seconds, f
	movlw	H'10'
	addwf	Seconds, f
	; Check if the upper bits reached 6
	swapf	Seconds, W
	andlw	H'0F'
	xorlw	d'6'
	btfss	STATUS, Z
	goto	END_TIMER_ISR
	; if it did then reset seconds and increment minutes
	clrf	Seconds
	incf	Minutes
END_TIMER_ISR
	; Do the DC cycles Check
	banksel	DC_Cycles
	movf	DC_Cycles, W
	xorlw	d'175'
	btfss	STATUS, Z
	goto	END_DC_TIMER_ISR
	; If the lower count reaches 175 then increment the High count
	clrf	DC_Cycles
	incf	DC_Cycles_H
	; Check if reached 3
	movf	DC_Cycles_H, W
	xorlw	d'3'
	btfss	STATUS, Z
	goto	END_DC_TIMER_ISR
	; if it did reach 3 then change the direction of the motor
	clrf	DC_Cycles_H
	bcf		PORTE, 2
	movf	PORTE, W
	xorlw	B'00000011'			; Inverts bit 0 and 1 only keeping everything else
	movwf	PORTE
	bsf		PORTE, 2
	goto	END_DC_TIMER_ISR
END_DC_TIMER_ISR
	return
					
;----------------------------------
; LCD related subroutines
;----------------------------------

;*****************
; Initialization *
;*****************
InitLCD
	bcf STATUS,RP0
	bsf E     ;E default high
	
	;Wait for LCD POR to finish
	call lcdLongDelay
	call lcdLongDelay
	call lcdLongDelay
	call lcdLongDelay

	;Ensure 8-bit mode first (no way to immediately guarantee 4-bit mode)
	; -> Send b'0011' 3 times
	movlw	b'00110011'
	call	WR_INS
	movlw	b'00110010'
	call	WR_INS

	; 4 bits, 2 lines, 5x7 dots
	movlw	b'00101000'
	call	WR_INS

	; display on/off
	movlw	b'00001100'
	call	WR_INS
	
	; Entry mode
	movlw	b'00000110'
	call	WR_INS

	; Clear ram
	movlw	b'00000001'
	call	WR_INS
	return
	
;*********************
; Clear LCD	Display  *
;*********************
ClrLCD
	movlw	B'00000001'
	call	WR_INS
    return
   
;************************
; Turn into 1 line mode *
;************************
OneLineMode
	movlw	B'00100000'
	call	WR_INS
	return

;************************
; Turn into 2 line mode *
;************************
TwoLineMode
	movlw	B'00101000'
	call	WR_INS
	return
	
;*****************
; Display Cursor *
;*****************
DisplayCursor
	movlw	B'00001110'
	call	WR_INS
	return

;**************
; Hide Cursor *
;**************
HideCursor
	movlw	B'00001100'
	call	WR_INS
	return
	
;**************
; Display OFF *
;**************
DisplayOFF
	movlw	B'00001000'
	call	WR_INS
	return
	
;**************
; Display ON  *
;**************
DisplayON
	movlw	B'00001100'
	call	WR_INS
	return
	
;***************************
; Write Instruction from W *
;***************************
WR_INS
	bcf		RS				;clear RS
	movwf	com				;W --> com
	andlw	0xF0			;mask 4 bits MSB w = X0
	movwf	PORTD			;Send 4 bits MSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	com,w
	andlw	0xF0			;1111 0010
	movwf	PORTD			;send 4 bits LSB
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	call	lcdLongDelay
	return
	
;*************************
; Write character from W *
;*************************
WR_DATA
	bsf		RS				
	movwf	dat
	movf	dat,w
	andlw	0xF0		
	addlw	4
	movwf	PORTD		
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	swapf	dat,w
	andlw	0xF0		
	addlw	4
	movwf	PORTD		
	bsf		E				;
	call	lcdLongDelay	;__    __
	bcf		E				;  |__|
	return
	
;******************
; Long delay loop *
;******************
lcdLongDelay
    movlw 	d'40'
   	movwf 	lcd_d2
LLD_LOOP
   	LCD_DELAY
   	decfsz 	lcd_d2,f
   	goto	LLD_LOOP
   	return
   
;----------------------------------
; Other subroutines
;----------------------------------

;*************
; Delay 0.5s *
;*************
HalfS	
	local	HalfS_0
      movlw 0x88
      movwf COUNTH
      movlw 0xFF
      movwf COUNTM
      movlw 0x02
      movwf COUNTL

HalfS_0
      decfsz COUNTH, f
      goto   $+2
      decfsz COUNTM, f
      goto   $+2
      decfsz COUNTL, f
      goto   HalfS_0

      goto $+1
      nop
      nop
	return
	

;----------------------------------
; Keypad related subroutines
;----------------------------------

;****************
; Poll for Data *
;****************

PollKeys
	btfss	DAV     	;Wait until data is available from the keypad
    goto	$-1 
    swapf	PORTB,W     ;Read PortB<7:4> into W<3:0>
    andlw	0x0F		;clear the other bits in W
    
    btfsc	DAV			;Wait until key is released
    goto	$-1
    return


	;END OF FILE
	END	
