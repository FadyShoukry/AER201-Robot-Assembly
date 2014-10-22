;====================================== 
; MICROCONTROLLER DECLARATION
;======================================
	list p=16f877
	#include <p16f877.inc>
	
;======================================
; MACROS
;======================================
Stepf macro	n
	banksel	PORTC
	movlw	n
	movwf	tmp_count
	local	Loop
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
;======================================
; CONFIGURATION
;======================================
	__CONFIG _CP_OFF & _WDT_OFF & _BODEN_ON & _PWRTE_ON & _HS_OSC & _WRT_ENABLE_ON & _CPD_OFF & _LVP_OFF
	
	#define KeyA	H'03'
	#define KeyB	H'07'
	#define DAV 	PORTB,1

cblock	0x70
	lcd_d1
	lcd_d2
	tmp_count
	endc
;;;;;;;;;;;;;;;;;;;;;;;;;;
	ORG 0x0000
	goto init
	
init
	banksel	TRISC
	clrf	TRISC
	movlw   b'11110010'    	; Set RB<7:4> and RB1 to input
    movwf  	TRISB
	clrf	TRISA
	clrf	TRISD
	clrf	TRISE
	banksel	PORTC
	clrf	PORTC
	clrf	PORTB
Loop
	btfss	DAV     	;Wait until data is available from the keypad
    goto	$-1 
    
    swapf	PORTB,W     ;Read PortB<7:4> into W<3:0>
    andlw	0x0F		;clear the other bits in W

    btfsc	DAV			;Wait until key is released
    goto	$-1
	
	xorlw	KeyA
	btfss	STATUS,Z
	goto	StepForward
	goto	StepBackward
StepForward
	Stepf 	d'10'
	goto	Loop
StepBackward
	Stepb	d'10'
	goto 	Loop
	
	

	
;******************************
; LCD_DELAY: Delay for ~102us *
;******************************
LCD_DELAY macro
		movlw	0xFF				;move the required value of cycles to W
		movwf	lcd_d1				;move the required value from W to a register
		decfsz	lcd_d1,f			;decrement the value and skip the next inst. when 0
		goto	$-1					;return to the prev. inst. (decrement), skipped if 0
		endm
		
;*********************
; Stepper delay loop *
;*********************
StepperDelay
    movlw 	d'15'
   	movwf 	lcd_d2
SD_LOOP
   	LCD_DELAY
   	decfsz 	lcd_d2,f
   	goto	SD_LOOP
   	return
   	
   	
   	END