; =============================================================================
; Global Constants     
; =============================================================================



.ifndef GLOBAL_CONSTANTS
; ------------------------------------------------------------------------------

; DMARKER (str) - String and Address markers for debugging binaries.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DEBUG_MARKERS = 1
.macro DMARKER str
	.ifdef DEBUG_MARKERS
		.byte str
		.word * + 3
		.byte $00
	.endif
.endmacro

; PSTRING (str) - Pascal style string encoding, length byte followed by string.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.macro PSTRING str
	.byte .strlen(str), str
.endmacro

; ANIMPTR (ptrFrameData, length, duration)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.macro ANIMPTR ptrFrameData, length, duration
	.addr ptrFrameData
	.byte length, duration
.endmacro

; FRAMEDATA (xOfs, yOfs, tileIdx, attrib)
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.macro FRAMEDATA xOfs, yOfs, tileIdx, attrib
	.byte <(yOfs), <(tileIdx), <(attrib), <(xOfs)
.endmacro

; PPU Registers 
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
PPUCTRL 	:= $2000
PPUMASK 	:= $2001
PPUSTATUS 	:= $2002
OAMADDR 	:= $2003
PPUSCROLL 	:= $2005
PPUADDR 	:= $2006
PPUDATA 	:= $2007
OAMDMA 		:= $4014
JOY1		:= $4016		; Joypad 1 Data Register
JOY2	    := $4017        ; Joypad 2 Data Register

; Color Names
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

COLOR_BLACK		= $0F


; Bank Names
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
BANK_AUDIO		= 0
BANK_CHR_0		= 1
BANK_CHR_1      = 2
BANK_GAME       = 3
BANK_STAGEDATA  = 4
BANK_SPRITES    = 5
BANK_NTPAL		= 6
BANK_FIXED		= 7
BANK_ID  		:= $8000	; Bank Identifier

; Button Input Mask Defines
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
JOY_RIGHT       = %00000001
JOY_LEFT        = %00000010
JOY_DOWN        = %00000100
JOY_UP			= %00001000
JOY_START       = %00010000
JOY_SELECT      = %00100000
JOY_B           = %01000000
JOY_A           = %10000000

; PpuOverride Flag
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SKIP_PPUCTRL	= $80	; C : (0) Normal (1) Skip PPUCTRL NMI Update
SKIP_PPUMASK	= $40	; M : (0) Normal (1) Skip PPUMASK NMI Update
; OAM Attribute Flags
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
OAM_FLIP_V		= %10000000
OAM_FLIP_H		= %01000000

; Program Modes
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
M_BOOT			= $00
M_MENU			= $10
M_INITGAME		= $20
M_STAGELOAD		= $30
M_STAGEINTRO	= $40
M_STAGERUN		= $50

; PlayerChars Flags
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
P1_ACTIVE 		= $08
P2_ACTIVE		= $80

; Ent_Flags
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
ENT_ACTIVE		= %10000000		; Ent is active, do process

; Ent_MoveFlags
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
MOVE_LEFT		= %10000000		; Ent move left (negative X)
MOVE_UP			= %01000000		; Ent moving up (negative Y)
MOVE_HMOVE		= %00100000		; Ent actively walking.
MOVE_VMOVE		= %00010000		; Ent holding jump.
MOVE_GROUNDED	= %00001000		; Ent is on the ground

; Sound Effect Names
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

; ------------------------------------------------------------------------------
GLOBAL_CONSTANTS = 1
.endif