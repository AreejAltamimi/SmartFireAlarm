INCLUDE 89C52-Modif.mc

;     ---------  FireAlarm  ----------------
;This program receives an interrupt Int0 from fire detection cct and display a message on lcd,buzzer and sends SMS.
; Also another interrupt Int1 and battery monitor cct and displays a message, and buzzer.
;The program monitors a RTC chip for time and date
; Serial communications with GSM modem TC35

; General Notes:
	; IE interrupt Enable reg      B7:B0  7=EA-All, 6=Resv'd, 5=ET2-Timer2, 4=ES-Serial
	;			        3=ET1-Timer1, 2=EX1-External 1, 1=ET0 Timer0 , 0 = ET0 External 0
       	; The External Interrupts INT0 and INT1 can each be either level-activated or transition-activated, depending on bits
	; IT0 and IT1 in Register TCON.
	; The flags that actually generate these interrupts are the IE0 and IE1 bits in TCON.
	; Interrupt vectors:
; (0003h):		; Adress of INT0 (IE0)
; (000Bh):		; Adress of Interrupt Timer 0 (TF0)
; (0013H)		; External 1 (IE1)
; (001BH)		; Timer 1  (TF1)
; (0023h):  		; Serial Interrupt
; (002Bh):		; Timer2 (TF2 or EXF2)
; (0000H):		; Reset system


; Bytes
Key_Pad	EQU	P0
LCD_Data	EQU	P1
LCD_Cont_Buz	EQU	P2
Controls	EQU	P3

Seconds	EQU	$20
Minutes	EQU	$21
Hours		EQU	$22
Day		EQU	$23
Date		EQU	$24
Month		EQU	$25
Year		EQU	$26
Timer_Cntrl	EQU	$27
PW1		EQU	$28		; 56 bytes in RAM from 28H to 5F
PW2		EQU	$29
PW3		EQU	$2A
CHKPW1	EQU	$2B		; a value if $AA-$55-$AA-$55 then PW is set or battery out
CHKPW2	EQU	$2C

PhoneNu01	EQU	$2D		; Phone Nu 01 to $0F in 16 locations $30:#3F
PhoneNu02	EQU	$2E  		; Last data Location , digits end with FF
PhoneNu03	EQU	$2F
PhoneNu04	EQU	$30
PhoneNu13	EQU	$39
PhoneNu14	EQU	$3A
PhoneNu15	EQU	$3B
PhoneNu16	EQU	$3C		; Last Memory locations 15 digits + $FF

RxIn001	EQU	$40
RxIn002	EQU	$41
RxIn003	EQU	$42
RxIn004	EQU	$43
RxIn005	EQU	$44
RxIn006	EQU	$45
RxIn007	EQU	$46
RxIn008	EQU	$47
RxIn009	EQU	$48
RxIn010	EQU	$49
RxIn011	EQU	$4A
RxIn012	EQU	$4B
RxIn013	EQU	$4C
RxIn014	EQU	$4D
RxIn015	EQU	$4E
RxIn016	EQU	$4F

ScrPad1	EQU	$50		; Scratch Pad
ScrPad2	EQU	$51
ScrPad3	EQU	$52
ScrPad4	EQU	$53
RxInCount	EQU	$54		; Counts Serial received bytes
LoopCount	EQU	$55
TimeDate	EQU	$56
SaveR0	EQU	$57

ReadByteCnt	EQU	$58		; Read time bytes from Timer
WByteCount	EQU	$59		; Counts Sent Timer bytes
KeyCntr	EQU	$5A		; Display control flag
RxCurAdd	EQU	$5B		; Bytes to send
;RxByteCnt	EQU	$5C		; Bytes Received Count

RxByte		EQU	$5D

;EndOfTxFlg	EQU	$5E		; $FF=Transmission completed flag, any other value send more bytes
Tx_Flag	EQU	$5F		; One Byte Sent completed=FF otherwise any value

DispTxt1	EQU	$60		; LCD line text, LCD adress in B reg, next 16 location till 5F
DispTxt2	EQU	$61
DispTxt3	EQU	$62
DispTxt4	EQU	$63
DispTxt5	EQU	$64
DispTxt6	EQU	$65
DispTxt7	EQU	$66
DispTxt8	EQU	$67
DispTxt9	EQU	$68
DispTxt10	EQU	$69
DispTxt11	EQU	$6A
DispTxt12	EQU	$6B
DispTxt13	EQU	$6C
DispTxt14	EQU	$6D
DispTxt15	EQU	$6E
DispTxt16	EQU	$6F

UsrInput1	EQU	$70
UsrInput2	EQU	$71
UsrInput3	EQU	$72
UsrInput4	EQU	$73
UsrInput5	EQU	$74
UsrInput6	EQU	$75
UsrInput7	EQU	$76
UsrInput8	EQU	$77
UsrInput9	EQU	$78
UsrInput10	EQU	$79

UsrInput16	EQU	$7F


; Bits
Col_0		EQU	P0.0		; KBD Col0
Col_1		EQU	P0.1		; KBD Col1
Col_2		EQU	P0.2		; KBD Col2
Col_3		EQU	P0.3		; KBD Col3
Row_0		EQU	P0.4		; KBD Row0
Row_1		EQU	P0.5		; KBD Row1
Row_2		EQU	P0.6		; KBD Row2
Row_3		EQU	P0.7		; KBD Row3

Buzzer		EQU	P2.0
LCD_Ena	EQU	P2.5		; LCD controls
LCD_R_NW	EQU	P2.6
LCD_RS	EQU	P2.7

N_Alarm	EQU	P3.2
ModActivate	EQU	P3.3		; Activate TC35
SQW_OUT	EQU	P3.4
SCL		EQU	P3.5
SDA		EQU	P3.6
LowBat	EQU	P3.7

KeyTimes	EQU	150

NoGSM		EQU	F0
PW_OK	EQU	UF

; LCD Lines:
Line1		EQU	$0		; 00h :0fh
Line2		EQU	$40		; 40H :4FH

;Skip the interrupt vectors:
(0000h):
	AJMP Initialize

;*************************************************************************

(0003h):		; Adress of INT0 (IE0)	A fire occured, Set alarm and send SMS
	MOV	R5,#5
	ACALL	Wait_100ms
	IF BIT	N_Alarm THEN	RETI	; Be sure the fire trigger holds for some time
	SETB	Buzzer
	ACALL	ClrDispTxtMem	; Puts white space " " =$20 in DispTxt1:16 - ret  no effect
	MOV	R2,#8
	MOV	DPTR,#TxtFire
	MOV	B,#Line2
	ACALL	ShowTxt		; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line# in B

	MOV	R5,#5
	LL2:	ACALL	Wait_1s		; wait 5 sec before sending SMS
	DJNZ	R5,LL2
	AJMP	SendSMS


;*************************************************************************
; Serial Interrupt
(0023h):
ChkSIntrrpt:
	IF BIT RI THEN				; It is RI flag, Bytes received, respond only to T1,R1
		CLR RI				; Clearing the Receive Interrupt (RI) bits
		MOV	A,SBUF 		; Get the received value
		IF	A = #$0A THEN RETI	; Skip CR charachter
		IF	A = #$0D THEN RETI	; Skip LF charachter
		MOV	R0,RxCurAdd		; get next saving address
		INC	R0
		MOV	@R0,A			; Save Rx'd byte
		MOV	RxCurAdd,R0
		INC	RxInCount		; count serial received bytes
	END IF
	RETI					; Return from the interrupt routine

;*************************************************************************

Initialize:
	; IE interrupt Enable reg      B7==B0  7=EA-All, 6=Resv;d, 5=ET2-Timer2, 4=ES-Serial, 3=ET1-Timer1
	;			        2=EX1-External 1, 1=ET0 Timer0 , 0 = ET0 External 0
	; The External Interrupts INT0 and INT1 can each be either level-activated or transition-activated, depending on bits
	; IT0 and IT1 in Register TCON. The flags that actually generate these interrupts are the IE0 and IE1 bits in TCON.

	CLR	Buzzer
	MOV	SP,#$C0
	Init_LCD:
	CLR	LCD_Ena		; Clear LCD ENA bit
	ACALL	Wait_100ms		; 90 m.s. from power up to complete internal Init

	MOV	B,#!00111000		; 8bit, 2line, 5X8 display
	ACALL	Store_CMD		; Execute: WAIT IS ADDED TO STORE_CMD INSTEAD
	MOV	B,#!00001110		; Set display on,cursor on, blinking off
	ACALL	Store_CMD		; Execute
	MOV	B,#!00000110		; Set Increment +1 mode, move cursor off
	ACALL	Store_CMD		; Execute
	MOV	B,#1			; Clear entire display
	ACALL	Store_CMD		; Execute

			; Initialize Serial Comm
	MOV	IE,#10010001b		; Interrupt Enable B7>>B0 "All, x , T2 , Ser, T1, Ext1, T0,Ext0"
	MOV	TMOD , #20H		; Timer Mode 2, Auto reload
	MOV	SCON,#40H 	   	; Set mode 1, serial port config to 8-bit
	MOV	TH1 , #0FDH    		; Load reload value for 19.2kps, based on 11.5092Mhz Crystal from temp controller
	MOV	PCON,#80H    		; SMOD=1, enable double data rate

	CLR	TI
	CLR	RI
	MOV	RxInCount,#0		; Clear received bytes count.
	MOV	RxCurAdd,#RxIn001	; Point to list start

	SETB	ModActivate		; Activate TC module
	ACALL	Wait_1S
	CLR	ModActivate

	Init_DS:
		; ReadTimer Reads DS1307 from timer adress in R3 with Bytes Count in ReadByteCnt, save Mem start adress in R0 No display Update
	MOV	R3,#0			; Start with Time data @ 00
	MOV	ReadByteCnt,#29	; Get all, time and PW and phone Nu
	MOV	R0,#Seconds		; Mem adress
	ACALL	ReadTimer

	MOV	WByteCount,#1	; just update the seconds to run the clock
	MOV	R4,#0
	MOV	R1,#Seconds
	ACALL	Write2DS		; update DS1307 with Bytes Count in WByteCount, timer adress in R4, Mem start adress in R1

	IsPwValid:			; check if ChkPw1 contain AA and ChkPw2 has 55 then ok or battery was out
	MOV	A,#$AA
	CJNE	A,CHKPW1,NotValid
	CPL	A			; Chk against $55
	CJNE	A,CHKPW2,NotValid
	SJMP	MainProg		; Valid PW, LEAVE PW DATA
	NotValid:			; Clear PW to 000
		MOV	PW1,#0
		MOV	PW2,#0
		MOV	PW3,#0
	MOV	TimeDate,#12

	MOV	DPTR,#TxtProj
	MOV	R2,#16
	MOV	B,#Line1
	ACALL	ShowTxt		; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line# in B
	ACALL	Wait_1s

MainProg:
	ACALL	Get_Key			; 0.13671 sec, result in Acc "A"
	IF A = #$F THEN			; User pressed " * " SW, Get Command
		ACALL	ClrDispTxtMem	; Puts white space " " =$20 in DispTxt1:16 - ret
		MOV	DispTxt1,#"?"		; Prompt user to input a command
		MOV	B,#Line2
		ACALL	UpdateDisplay		; Updates LCD Line No in B from Locations DisplTxt1 to DisplTxt16
	LM00:	MOV	R5,#KeyTimes		; Loop many times waiting for order
			ACALL	Get_Key
			IF A = #$A THEN AJMP SetTime 	; F1 set time Goto Function and return from there to main to save stack
			IF A = #$B THEN AJMP SetDate	; F2  set date
			IF A <> #$FF THEN SJMP MainProg	; if user pressed any other key, cancel process
		DJNZ	R5,LM00
		ACALL	ClrLine2M-Dis		; Clears DispTxt1:16 and Clear display Line2
	END IF			; No key
		; ReadTimer Reads DS1307 from timer adress in R3 with Bytes Count in ReadByteCnt, save Mem start adress in R0 No display Update
	MOV	R3,#0			; Start with Time data @ 00
	MOV	ReadByteCnt,#8		; Get time and date
	MOV	R0,#Seconds		; Mem adress
	ACALL	ReadTimer	; Reads DS1307 from timer adress in R3 with Bytes Count in ReadByteCnt, save Mem start adress in R0 No display Update

	IF TimeDate =  #2 THEN ACALL DispDate		; will display date and time then return
	IF TimeDate =  #25 THEN ACALL DispTime

	DJNZ	TimeDate,MainProg
	MOV	TimeDate,#41
	AJMP	MainProg

;			; ==============SubPrograms & Subrotines =================

SetTime:
	MOV	R2,#16			; copy 16 chr to display "  Time: HHMM  "
	MOV	DPTR,#TxtTime	; get adress
	ACALL ShowIt1Sec		; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line2 for 1 S then Del
	MOV	R1,#UsrInput1		; input save adress
	MOV	R0,#DispTxt1		; get Display address
	FOR	R4 =  #0 to #KeyTimes	; if user did not enter new time, exit SUB
		ACALL	Get_Key
		ACALL	Wait_100ms
		IF A <> $FF THEN		; User pressed a key
			IF A > #9 THEN AJMP WrongEntry	; not a valid key, Exit
			MOV	@R1,A		; Save user input
			ADD	A,#$30		; Convert number to ASCII
			MOV	@R0,A		; show number
			MOV	B,#Line2
			ACALL	UpdateDisplay		; Updates LCD Line No in B from Locations DisplTxt1 to DisplTxt16
			INC	R0
			INC	R1
			MOV	R4,#0		; reset waiting for user input
			IF	R1 = #UsrInput3 THEN	; Hours completed, check it
				MOV	A,UsrInput1	; Load tens
				SWAP	A		; Move it up
				ADD	A,UsrInput2	; Add units
				IF A > #$23 THEN AJMP WrongEntry		; Hour 0:23
				MOV	ScrPad1,A				; Save Hours
			END IF
			IF	R1 = #UsrInput5 THEN				; Minutes Completed
				MOV	A,UsrInput3
				SWAP	A
				ADD	A,UsrInput4
				IF A > #$59 THEN AJMP WrongEntry		; Minutes 0:59
				MOV	Minutes,A				; Now HH SS are OK set DS1307
				MOV	Hours,ScrPad1			; Seconds is cleard in Write2DS
									; Point to DS1307 data area
				MOV	WByteCount,#3		; Save Hrs and minutes and seconds
				MOV	R4,#0
				MOV	R1,#Seconds
				ACALL	Write2DS	; update DS1307 with Bytes Count in WByteCount, timer adress in R4, Mem start adress in R1
				SJMP	TimeDone
			END IF
		END IF
	NEXT
	TimeDone:	ACALL	ClrLine2M-Dis
	AJMP	MainProg


SetDate:
	MOV	R2,#16			; copy 16 chr to display " Date: DDMMYY  "
	MOV	DPTR,#TxtDate		; get adress
	ACALL ShowIt1Sec		; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line2 for 1 S then Del
	MOV	R1,#UsrInput1		; save input
	MOV	R0,#DispTxt1		; get Display address
	FOR	R4 =  #0 to #KeyTimes	; if user did not enter new time, exit
		ACALL	Get_Key
		ACALL	Wait_100ms	; Some delay
		IF A <> $FF THEN		; User pressed a key
			IF A > #9 THEN AJMP WrongEntry	; not a valid key, Exit
			MOV	@R1,A		; Save user input
			ADD	A,#$30		; Convert number to ASCII
			MOV	@R0,A		; show number
			MOV	B,#Line2
			ACALL	UpdateDisplay		; Updates LCD Line No in B from Locations DisplTxt1 to DisplTxt16
			INC	R0
			INC	R1
			MOV	R4,#0		; reset waiting for user input
			IF	R1 = #UsrInput3 THEN		; Date completed, check it
				MOV	A,UsrInput1		; tens
				SWAP	A			; move it left
				ADD A,UsrInput2
				IF A > #$31 THEN AJMP WrongEntry	; days 0:31
				MOV	ScrPad1,A
			END IF
			IF	R1 = #UsrInput5 THEN			; Month completed, check it
				MOV	A,UsrInput3
				SWAP	A
				ADD	A,UsrInput4
				IF A > #$12 THEN AJMP WrongEntry	; Month 0:12
				MOV	ScrPad2,A
			END IF
			IF	R1 = #UsrInput7 THEN			; Years completed
				MOV	A,UsrInput5
				SWAP	A
				ADD	A,UsrInput6
				MOV	ScrPad3,A			; Year are 0:99 Now DD MM YY are OK ,Ask Day
				SJMP	AskDay
			END IF
		END IF
	NEXT
	ACALL	ClrLine2M-Dis			; Entry not completed, Clears DispTxt1:16 and Clear display Line2-ret
	AJMP	MainProg
AskDay:
	MOV	R2,#16
	MOV	DPTR,#TxtDay
	ACALL	ShowIt1Sec	; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line2 for 1 S then Del
	FOR R4 = #0 TO #KeyTimes
		ACALL	Get_Key
		ACALL	Wait_100ms	; Some delay
		IF A <>  #$FF THEN
			IF A > #7 THEN AJMP WrongEntry
			IF A =  #0 THEN AJMP WrongEntry
			MOV	Day,A
			MOV	Date,ScrPad1
			MOV	Month,ScrPad2
			MOV	Year,ScrPad3
			SJMP	SaveDate
		END IF
	NEXT
	ACALL	ClrLine2M-Dis		; Clears DispTxt1:16 and Clear display Line2-ret
	AJMP	MainProg		; Entry Not completed
	SaveDate:
	ACALL	ClrLine2M-Dis
	MOV	WByteCount,#8	; Update Time/Date Info
	MOV	R4,#0			; DS Start Address
	MOV	R1,#Seconds
	ACALL	Write2DS 		; update DS1307 with Bytes Count in WByteCount, timer adress in R4, Mem start adress in R1
	AJMP	MainProg


WrongEntry:
	MOV	R2,#16			; copy 16 chr to display " Wrong  "
	MOV	DPTR,#TxtWrong	; Adress
	ACALL ShowIt1Sec		; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line2 for 1 S then Del
	AJMP	MainProg

			; ============== Subrotines =================

ReadTimer:	; Reads DS1307 from timer adress in R3 with Bytes Count in ReadByteCnt, save Mem start adress in R0 No display Update
	MOV	WByteCount,#0	; write 0 bytes , just set start adress for read operation
	MOV	A,R3			; no copy r3,r4, so copy r3>a then a>r4
	MOV	R4,A			; Copy timer  Address from R3 to R4
	ACALL	Write2DS	; update DS1307 with Bytes Count in WByteCount, timer adress in R4, no Mem start adress in R1
				; Returns with STOP Command
OneByte:
	ACALL	StartDS		; Start Command
	MOV	A,#!11010001		; device addr + Read
	ACALL	WDS8bit		; Writes 8 bits in A to DS and Chks for Ack,retrn with both LOW

	DoByteCount:		 	; Now read from ds no of bytes in ReadByteCnt
		MOV	R5, #8	; 8 bit counter
		ACALL	DsDel		; more delay after ack
	RT0:	SETB	SDA		; Make it input
		NOP
		SETB	SCL		; Clock
		ACALL	DsDel
		MOV	C,SDA		; Copy DATA BIT to carry
		RLC	A		; Most Significant bit first
		CLR	SCL
		NOP
	DJNZ	R5,RT0			; loop 8 bits

		MOV	@R0,A		; Save byte
		INC	R0		; next mem
		IF ReadByteCnt = #1 THEN SJMP NoAck
		CLR	SDA		; Send Ack
		JB	SDA,$		; be sure SDA is low
		ACALL	DsDel
		SETB	SCL		; ack bit clock
		ACALL	DsDel
		CLR	SCL		; clock LOW
		ACALL	DsDel
		DJNZ	ReadByteCnt,DoByteCount
	NoAck:
	SETB	SDA		; No Ack
	ACALL	DsDel
	SETB	SCL		; ack bit clock
	ACALL	DsDel
	CLR	SCL		; clock it
	ACALL	DsDel
	ACALL	StopDs		; Stop Command
	RET


Write2DS: 				; update DS1307 with Bytes Count in WByteCount, timer adress in R4, Mem start adress in R1
	MOV	Timer_Cntrl,#!10010000	; set control byte Out=1, X,X, SQWE=1 osc out ena, X,X, 0,0 freq=1Hz
	MOV 	Seconds,#0		; Clear CH bit (seconds.7) to enable oscillator and set seconds to 0
	ANL	Day,#7			; clear 5 MSBits
	ANL	Hours,#!00111111	; adj 12/24 hrs format to 24 Hrs
	ACALL	StartDs			; Start command and return both LOW
	MOV	A,#!11010000		; device addr + nWrite
	ACALL	WDS8bit 		; Send DS adress and write
	MOV	A,R4			; Timer write Adress
	ACALL	WDS8bit
	GetNext:
	IF WByteCount = #0 THEN SJMP WriteEnd
		DEC	WByteCount	; decrement count
		MOV	A,@R1		; Get next byte
		ACALL	WDS8Bit	; Write it
		INC	R1		; inc mem address
		SJMP	GetNext
	WriteEnd:
	ACALL	StopDs			; Stop Command
	RET

WDS8bit:				; Writes 8 bits in A to DS and Chks for Ack,Destroys R2
	MOV	R2,#8
	WDS0:
		RLC	A		; Most Significant bit first
		MOV	SDA,C		; copy bit to data line
		ACALL	DsDel
		SETB	SCL		; clock it
		ACALL	DsDel
		CLR	SCL		; clock LOW
		ACALL	DsDel
		DJNZ	R2,WDS0
	SETB	SDA			; Make SDA input ready 4 receiving ack
	ACALL	DsDel
	SETB	SCL			; ack bit clock
	ACALL	DsDel
	CLR	SCL			; clock Low
	RET

StartDS:
	SETB	SDA			; Set Hi fot DS1307
	SETB	SCL
	CLR	SDA
	NOP
	NOP
	CLR	SCL
	NOP				; Delay After Start
	NOP
	RET

StopDS:
	SETB	SCL
	NOP
	NOP
	SETB	SDA
	NOP				; Delay After stop
	NOP
	RET

DispDate:				; writes "DOW dd mm yyyy" on LCD line 1 DOW=Sat ,Sun Tue etc
	ACALL	ClrDispTxtMem	; Clear DispTxt1 to 16
	MOV	R1,#DispTxt1		; get display txt adress
	MOV	A,Day			; 1:7=SUN:sat
	DEC	A
	MOV	B,#3
	MUL	AB			; Get relative starting string
	MOV	B,A			; Save copy in B
	MOV	DPTR,#DayOfWeek	; List adress
	L3tims:
		MOVC	A,@A+DPTR		; get 1st character
		MOV	@R1,A			; Store Chr in list
		MOV	A,B			; restore rel string
		INC	DPTR
		INC	R1			; Point to next
		CJNE	R1,#DispTxt4,L3Tims	; 3 charts only
	MOV	DispTxt4,#":"
	MOV	A,date
	ACALL	BCD2ASCii		; converts a BCD byte in A to 2 Ascii in R5(units) and R6(tens)
	MOV	DispTxt5,R6
	MOV	DispTxt6,R5
	MOV	DispTxt7,#"-"
	MOV	A,Month
	ACALL	BCD2ASCii		; converts a BCD byte in A to 2 Ascii in R5(units) and R6(tens)
	MOV	DispTxt8,R6
	MOV	DispTxt9,R5
	MOV	DispTxt10,#"-"
	MOV	DispTxt11,#"2"
	MOV	DispTxt12,#"0"
	MOV	A,Year
	ACALL	BCD2ASCii		; converts a BCD byte in A to 2 Ascii in R5(units) and R6(tens)
	MOV	DispTxt13,R6
	MOV	DispTxt14,R5
	MOV	B,#Line1
	ACALL	UpdateDisplay		; Updates LCD Line No in B from Locations DisplTxt1 to DisplTxt16
	RET

DispTime:				; writes "Time: hh mm ss" on LCD line 1

	MOV	DPTR,#TxtTime
	MOV	R2,#7
	MOV	R1,#DispTxt1
	MOV	B,#Line1
	ACALL	ShowTxt		; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line# in B

	MOV	A,Hours		; get Hour data
	ACALL	BCD2ASCii		; conv to ascii in R5,R6
	MOV	DispTxt8,R6
	MOV	DispTxt9,R5
	MOV	DispTxt10,#"-"		; Writes "hh "
	MOV	A,Minutes
	ACALL	BCD2ASCii		; converts a BCD byte in A to 2 Ascii in R5(units) and R6(tens)
	MOV	DispTxt11,R6
	MOV	DispTxt12,R5
	MOV	DispTxt13,#":"
	MOV	A,Seconds		; Get seconds data
	ACALL	BCD2ASCii		; converts a BCD byte in A to 2 Ascii in R5(units) and R6(tens)
	MOV	DispTxt14,R6
	MOV	DispTxt15,R5
	MOV	B,#Line1
	ACALL	UpdateDisplay		; Updates LCD Line No in B from Locations DisplTxt1 to DisplTxt16
	RET

	; --------

ClrDispTxtMem:			; Puts white space " " =$20 in DispTxt1:16 - ret
	MOV	SaveR0,R0		; save R0 - No Push Command for Rx
	MOV	R0,#DispTxt16
	CDM0:	MOV	@R0,#" "
		DEC	R0
		CJNE	R0,#DispTxt1-1,CDM0
	MOV	R0,SaveR0		; recall  R0
	RET

ShowIt1Sec:			; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line2 for 1 S then Del
	MOV	B,#Line2
	ACALL	ShowTxt
	ACALL	Wait_1S
	SJMP	ClrLine2M-Dis	; Clear Line 2 then return from there

ShowTxt:				; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line# in B
	ACALL	CopyTxt
	SJMP	UpdateDisplay		; Display the text, return from there

CopyTxt:				; Copy a number of Chrs in R2 From address in DPTR to dispTxt area
	MOV	SaveR0,R0		; save R0 - No Push Command for Rx
	MOV	R0,#DispTxt1
	PUSH	A
	NextChr:
	CLR	A
	MOVC	A,@A+DPTR
	MOV	@R0,A
	INC	R0
	INC	DPTR
	DJNZ	R2,NextChr
	MOV	R0,SaveR0		; recall  R0
	POP	A
	RET


ClrLine2M-Dis:				; Clears DispTxt1:16 and Clear display Line2
	ACALL	ClrDispTxtMem
	MOV	B,#Line2		; SET display adress $40 Command

UpdateDisplay:				; Updates LCD Line No in B from Locations DisplTxt1 to DisplTxt16
	SETB	B.7			; Make command goto adress
	ACALL Store_CMD		; store Goto Address
	MOV	SaveR0,R0		; save R0 - No Push Command for Rx
	FOR R0 = #DispTxt1 to #DispTxt16	; Point to list start
		MOV B,@R0		; Output Character
		ACALL Store_data	; store data
	NEXT
	MOV	R0,SaveR0		; recall  R0
	RET
			;  ---------------------------------

Store_CMD:				; writes a Command to LCD
	CLR	LCD_RS		; Command Reg
	CLR	LCD_R_NW		; Write mode
	NOP				; 5 ns needed
	SETB	LCD_Ena		;
	MOV	LCD_Data,B		; Put Command on data buss
	NOP				; DELAY > 175 ns
	CLR	LCD_Ena
	NOP
	ACALL wait_80			; Wait Execution time	39uS except Go Home And Clear display 1.53ms
	RET

Store_Data:				; writes a character to LCD @ current location
	SETB	LCD_RS		; Data Reg
	CLR	LCD_R_NW		; Write mode
	NOP
	SETB	LCD_Ena		;
	MOV	LCD_Data,B		; Put data on data buss
	NOP				; DELAY > 175 ns
	CLR	LCD_Ena
	NOP
	ACALL wait_80			; Wait Execution time >43us
	RET

		;----------------
BCD2Ascii:				; converts a BCD byte in A to 2 Ascii in R5(units) and R6(tens)
	IF	A > #60H THEN		; unvalid number max is 59
		MOV	R5,#" "
		MOV	R6,#" "
		RET
	END IF
	MOV	B,A			; Save Original Number
	ANL	A,#0FH			; Clear Left Nibble (Tens nibble)
	ORL	A,#30h			; Convert Units to ascii
	MOV	R5,A			; save Units
	MOV	A,B			; Get BCD again
	SWAP	A			; Get Tens
	ANL	A,#0FH			; Clear Left Nibble (Units)
	ORL	A,#30h			; Conv Tens to Asii
	MOV	R6,A			; Save Tens
	RET

	; --------------------------------------
Send2TC35:				; Sends text string in DPTR to TC35 module, text ends with $FF byte
	MOV	RxCurAdd,#RxIn001	; Reset the save index
	MOV	RxInCount,#0		; reset count serial received bytes

	Next00:
	CLR	A
	MOVC	A,@A+DPTR
	CLR	TI
	IF A = #$FF THEN RET		; End of string
	MOV	Tx_Flag,#0		; set send next byte flag
	MOV	SBUF,A
	INC	DPTR
	WaitTI:
	IF BIT TI THEN
		CLR	TI
		SJMP Next00
	ELSE
		SJMP	WaitTI
	END IF

	; --------------------------------------
ChkGSM:
	MOV	R0,#RxIn016
	L16:
	MOV	@R0,#$FF
	DEC	R0
	CJNE	R0,#RxIn001-1,L16
	MOV	RxInCount,#0		; Init Rx count
	MOV	DPTR,#TxtECHOoff	; Send echo Off command and waitreply
	ACALL	Send2TC35
	AJMP	Wait_1S		; Expect "OK"reply in one second
	MOV	R2,RxInCount		; nu of received bytes
	MOV	DPTR,#RxIn001
	ACALL	ShowIt1Sec		; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line2 for 1 S then Del

	RET
	; -------------------------------------

SendSMS:					; Sends SMS and do nothing else

	SETB	ES				; Serial Interrupt
	SETB	TR1				; Timer 1 start (Serial Comm)
	ACALL	ChkGSM			; Checks and initialize module
;	IF BIT NoGSM THEN			; No GSM Module
;		MOV	R2,#16			; copy 16 chr to display txt " no gsm connected"
;		MOV	DPTR,#TxtNoGSM	; get adress
;		MOV	B,#Line2
;		ACALL	ShowTxt	; Copy a number of Chrs in R2 From address in DPTR to dispTxt area Show it To Line# in B
;	ELSE
		MOV	DPTR,#TxtCfg		; "at+cmgf=1" sets module to SMS mode
		ACALL	Send2TC35
		ACALL	wait_500mS		; wait for reply
		MOV	DPTR,#TxtSndSMS		; DB "at+cmgs=phone number"
		ACALL	Send2TC35		; reply is CR+LF+ >
		ACALL	wait_500mS		; wait for reply

		MOV	DPTR,#TxtMsg		; Send txt message

		ACALL	Send2TC35		; reply is sms + code
		MOV	R2,#16			; copy 16 chr to display txt
		MOV	DPTR,#TxtMsgSent	; get adress "message sent"
		MOV	B,#Line2
		ACALL	ShowTxt
;	END IF
	RETI



Get_Key:				; Checks Keyboard and saves result in Acc A = 136710 uSec (0.136S)
			#IF NOT DEBUGGING
	MOV	A,#$FF				; User did not press any Key
	FOR	KeyCntr = #0 TO  #100		; Loop 250 times to give enough time to press

		MOV	Key_Pad,#!11111110		;Chk Row_0
		ACALL	Wait_80
		IF NOT BIT Row_0 THEN MOV A,  #1
		IF NOT BIT Row_1 THEN MOV A,  #2
		IF NOT BIT Row_2 THEN MOV A,  #3
		IF NOT BIT Row_3 THEN MOV A,  #$A	;  User pressed " F1 "
		CJNE	A,#$FF,	_Key_Dn

		MOV	Key_Pad,#!11111101		;Chk Row_1
		ACALL Wait_80
		IF NOT BIT Row_0 THEN MOV A,  #4
		IF NOT BIT Row_1 THEN MOV A,  #5
		IF NOT BIT Row_2 THEN MOV A,  #6
		IF NOT BIT Row_3 THEN MOV A,  #$B ; User pressed F2 Key
		CJNE	A,#$FF,	_Key_Dn

		MOV	Key_Pad,#!11111011		;Chk Row_2
		ACALL Wait_80
		IF NOT BIT Row_0 THEN MOV A,  #7
		IF NOT BIT Row_1 THEN MOV A, #8
		IF NOT BIT Row_2 THEN MOV A,  #9
		IF NOT BIT Row_3 THEN MOV A, #$C	; User pressed  F3 Key
		CJNE	A,#$FF,	_Key_Dn

		MOV	Key_Pad,#!11110111		;Chk Row_3
		ACALL Wait_80
		IF NOT BIT Row_0 THEN MOV A,  #$F	; User pressed " * " Key
		IF NOT BIT Row_1 THEN MOV A,  #0
		IF NOT BIT Row_2 THEN MOV A,  #$E	; User pressed " # "
		IF NOT BIT Row_3 THEN MOV A,  #$D	; User pressed F4 Key
		CJNE	A,#$FF,	_Key_Dn
	NEXT
	RET
	_Key_Dn:
		MOV	Key_Pad,#$F0
		IF Key_Pad <> #$F0 THEN SJMP _Key_Dn		; Key released
		 #END IF
	RET


 ;  -------------------------------------------------------------------------------------------------
; Delay routines at 11.059Mhz Xstal has 1.085uS pet cycle
;  ------------------------------------------------------------------------------------------------
; destroys: 	R7
; time:		14 cyc.	(incl. ACALL/RET)=15.19uSec
; stack:	2		(incl. ACALL/RET)=4
DsDel:			#IF NOT DEBUGGING
		MOV	R7,#9	; 1 cycle
		DJNZ	R7,$
		#END IF
		RET
;  ------------------------------------------------------------------------------------------------
; destroys: 	R7
; time:		75 cyc.	(incl. ACALL/RET)=81.375 uSec
; stack:	2		(incl. ACALL/RET)=4
Wait_80:	 #IF NOT DEBUGGING
			MOV	R7,#35	; 1 cycle
			DJNZ	R7,$
		#END IF
		RET
;  ------------------------------------------------------------------------------------------------
; destroys: 	R7
; time:		461 cyc.	(incl. ACALL/RET)=500.185 uSec
; stack:	4		(incl. ACALL/RET)=4
Wait_500:	 #IF NOT DEBUGGING
			MOV	R7,#228
			DJNZ	R7,$		; R7--: If (R7 > 0) repeat
		 #END IF
		RET
;  ------------------------------------------------------------------------------------------------
; destroys: 	R7,R6
; time:		91574 cyc.	(incl. ACALL/RET)=100 ms
; stack: 4		(incl. ACALL/RET)=4
Wait_100ms:	 #IF NOT DEBUGGING
		MOV	R6,#97
	_loop100ms:	MOV	R7,#235
	L10:		NOP
			NOP
			DJNZ	R7,L10
		DJNZ	R6, _loop100ms
		 #END IF
		RET

;  ------------------------------------------------------------------------------------------------
; destroys: 	R7,R6
; time:		520970cyc.	(incl. ACALL/RET)=565 ms
; stack: 4		(incl. ACALL/RET)=4
Wait_500ms:	 #IF NOT DEBUGGING
		MOV	R6,#255
	_loop500ms:	MOV	R7,#255
	L50:		NOP
			NOP
			NOP
			NOP
			NOP
			NOP
			DJNZ	R7,L50
		DJNZ	R6, _loop500ms
		#END IF
		RET


;  ------------------------------------------------------------------------------------------------
; destroys: 	R6,R7
; time:		998460. cyc.	(incl. ACALL/RET)= 1.083 s
; stack: 4		(incl. ACALL/RET)=4
Wait_1s:	;		   #IF NOT DEBUGGING
	LCALL	Wait_500ms
	LCALL	Wait_500ms
	RET

; -----------------------------------------------------------------------------------------



	DayOfWeek:	DB "SUNMONTUEWEDTHUFRISAT"
	TxtTime:	DB " Time:   HHMM   "
	TxtDate:	DB " Date: DDMMYY   "
	TxtWrong:	DB " Wrong Entery!  "
	TxtPhone:	DB "Phone "
	TxtFire:	DB " Fire!! "
	TxtPW:		DB "Enter P.W. "
	TxtNuPw:	DB "New PW:"
	TxtConfirm:	DB "Confirm PW: "
	TxtWrongPW:	DB " !!Wrong PW!!   "
	TxtNoMatch:	DB "No Match!"
	TxtPwChngd:	DB " P.W. Changed!  "
	TxtNuPhone:	DB "Enter no then #"
	TxtNoGSM:	DB "No GSM Connected"
	TxtDay:	DB "Day? SUN=1 Sat=7"
	TxtProj:	DB "  !!CALL 998 !! "

	TxtECHOoff:	DB "ATE0", $0D, $0A, $FF
	TxtCfg:		DB "at+cmgf=1" , $0D, $0A, $FF		; Txt Msg Mode + CR + LF
	TxtSndSMS:	DB "at+cmgs=998", $0D, $0A, $FF	; Send phone number + CR + LF
	TxtMsg:	DB " !!Fire!! " , $1A, $08 , $FF 			; The acual message sent followed by ^Z code (End of transmission)
	TxtMsgSent:	DB "Msg sent to 998 "
