; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Bank 4 : Stage Data
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.include "defines.inc"

.export DP_StageNames, DP_StageData
.define BANK_NUMBER		$04		; Bank Identifier

.segment "BANK4"
.org $8000
.byte BANK_NUMBER

DP_StageNames:
.addr	D_STAGENAME00			; Level 0 - 1
.addr	D_STAGENAME01			; Level 0 - 2
.addr	D_STAGENAME02			; Level 0 - 3
.addr	D_STAGENAME03			; Level 0 - 4
.addr	D_STAGENAME04			; Level 0 - 5
.addr	D_STAGENAME10			; Level 1 - 1
.addr	D_STAGENAME11			; Level 1 - 2
.addr	D_STAGENAME12			; Level 1 - 3
.addr	D_STAGENAME13			; Level 1 - 4
.addr	D_STAGENAME14			; Level 1 - 5
.addr	D_STAGENAME20			; Level 2 - 1
.addr	D_STAGENAME21			; Level 2 - 2
.addr	D_STAGENAME22			; Level 2 - 3
.addr	D_STAGENAME23			; Level 2 - 4
.addr	D_STAGENAME24			; Level 2 - 5
.addr	D_STAGENAME30			; Level 3 - 1
.addr	D_STAGENAME31			; Level 3 - 2
.addr	D_STAGENAME32			; Level 3 - 3
.addr	D_STAGENAME33			; Level 3 - 4
.addr	D_STAGENAME34			; Level 3 - 5
.addr	D_STAGENAME40			; Level 4 - 1
.addr	D_STAGENAME41			; Level 4 - 2
.addr	D_STAGENAME42			; Level 4 - 3
.addr	D_STAGENAME43			; Level 4 - 4
.addr	D_STAGENAME44			; Level 4 - 5
.addr	D_STAGENAME50			; Level 5 - 1
.addr	D_STAGENAME51			; Level 5 - 2
.addr	D_STAGENAME52			; Level 5 - 3
.addr	D_STAGENAME53			; Level 5 - 4
.addr	D_STAGENAME54			; Level 5 - 5
.addr	D_STAGENAME60			; Level 6 - 1
.addr	D_STAGENAME61			; Level 6 - 2
.addr	D_STAGENAME62			; Level 6 - 3
.addr	D_STAGENAME63			; Level 6 - 4
.addr	D_STAGENAME64			; Level 6 - 5

D_STAGENAME00: PSTRING "TEST CHAMBER 00"	; Stage 00
D_STAGENAME01: PSTRING "TEST CHAMBER 01"	; Stage 01
D_STAGENAME02: PSTRING "****"				; Stage 02
D_STAGENAME03: PSTRING "****"				; Stage 03
D_STAGENAME04: PSTRING "****"				; Stage 04
D_STAGENAME10: PSTRING "----"				; Stage 10
D_STAGENAME11: PSTRING "----"				; Stage 11
D_STAGENAME12: PSTRING "----"				; Stage 12
D_STAGENAME13: PSTRING "----"				; Stage 13
D_STAGENAME14: PSTRING "----"				; Stage 14
D_STAGENAME20: PSTRING "----"				; Stage 20
D_STAGENAME21: PSTRING "----"				; Stage 21
D_STAGENAME22: PSTRING "----"				; Stage 22
D_STAGENAME23: PSTRING "----"				; Stage 23
D_STAGENAME24: PSTRING "----"				; Stage 24
D_STAGENAME30: PSTRING "----"				; Stage 30
D_STAGENAME31: PSTRING "----"				; Stage 31
D_STAGENAME32: PSTRING "----"				; Stage 32
D_STAGENAME33: PSTRING "----"				; Stage 33
D_STAGENAME34: PSTRING "----"				; Stage 34
D_STAGENAME40: PSTRING "----"				; Stage 40
D_STAGENAME41: PSTRING "----"				; Stage 41
D_STAGENAME42: PSTRING "----"				; Stage 42
D_STAGENAME43: PSTRING "----"				; Stage 43
D_STAGENAME44: PSTRING "----"				; Stage 44
D_STAGENAME50: PSTRING "----"				; Stage 50
D_STAGENAME51: PSTRING "----"				; Stage 51
D_STAGENAME52: PSTRING "----"				; Stage 52
D_STAGENAME53: PSTRING "----"				; Stage 53
D_STAGENAME54: PSTRING "----"				; Stage 54
D_STAGENAME60: PSTRING "----"				; Stage 60
D_STAGENAME61: PSTRING "----"				; Stage 61
D_STAGENAME62: PSTRING "----"				; Stage 62
D_STAGENAME63: PSTRING "----"				; Stage 63
D_STAGENAME64: PSTRING "----"				; Stage 64

DP_StageData:
	.addr	D_STAGE00	
	.addr	D_STAGE01
	.addr	D_STAGE02
	.addr	D_STAGE03
	.addr	D_STAGE04
			
	.addr	D_STAGE10
	.addr	D_STAGE11
	.addr	D_STAGE12
	.addr	D_STAGE13
	.addr	D_STAGE14
			
	.addr	D_STAGE20
	.addr	D_STAGE21
	.addr	D_STAGE22
	.addr	D_STAGE23
	.addr	D_STAGE24
			
	.addr	D_STAGE30
	.addr	D_STAGE31
	.addr	D_STAGE32
	.addr	D_STAGE33
	.addr	D_STAGE34
				
	.addr	D_STAGE40
	.addr	D_STAGE41
	.addr	D_STAGE42
	.addr	D_STAGE43
	.addr	D_STAGE44
			
	.addr	D_STAGE50
	.addr	D_STAGE51
	.addr	D_STAGE52
	.addr	D_STAGE53
	.addr	D_STAGE54
			
	.addr	D_STAGE60
	.addr	D_STAGE61
	.addr	D_STAGE62
	.addr	D_STAGE63
	.addr	D_STAGE64

D_STAGE00: .include "stagemaps/stage00.asm"
D_STAGE01: .include "stagemaps/stage01.asm"
D_STAGE02:
D_STAGE03:
D_STAGE04:

D_STAGE10:
D_STAGE11:
D_STAGE12:
D_STAGE13:
D_STAGE14:

D_STAGE20:
D_STAGE21:
D_STAGE22:
D_STAGE23:
D_STAGE24:

D_STAGE30:
D_STAGE31:
D_STAGE32:
D_STAGE33:
D_STAGE34:

D_STAGE40:
D_STAGE41:
D_STAGE42:
D_STAGE43:
D_STAGE44:

D_STAGE50:
D_STAGE51:
D_STAGE52:
D_STAGE53:
D_STAGE54:

D_STAGE60:
D_STAGE61:
D_STAGE62:
D_STAGE63:
D_STAGE64: