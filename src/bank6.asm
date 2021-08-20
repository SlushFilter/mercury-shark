; =============================================================================
; Bank 6 : Graphics Data
; =============================================================================
.include "defines.inc"

.export DP_Nametables, D_Palettes
.define BANK_NUMBER		$06		; Bank Identifier
.segment "BANK6"
.org $8000
.byte BANK_NUMBER

; =============================================================================
; DP_Nametables :
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Pointers to RLE compressed nametables.
; =============================================================================
DP_Nametables:
.addr NT_SPLASH				; [00] Logo / Splash Screen
.addr NT_MENU               ; [01] Title Screen
.addr NT_PSELECT            ; [02] Player Select Screen

NT_SPLASH: 		.incbin "nametables/splash.nt"
NT_MENU:		.incbin "nametables/title.nt"
NT_PSELECT:		.incbin "nametables/player-select.nt"

; =============================================================================
; D_Palettes :
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; A list of 3 ColorSets, to be used with a foreground or background set.
; 
; 1     : Color of BG/FG Colors 1
; 2     : Color of BG/FG Colors 2
; 3     : Color of BG/FG Colors 3
; =============================================================================

D_Palettes:

.byte $00, $10, $20	; [0] Greyscale
.byte $02, $12, $22	; [1] Blues
.byte $04, $14, $24	; [2] Magentas
.byte $06, $16, $26	; [3] Reds
.byte $07, $17, $27	; [4] Oranges
.byte $08, $18, $28	; [5] Yellows/Pukes
.byte $0A, $1A, $2A	; [6] Greens
.byte $0C, $1C, $2C	; [7] Cyans

.byte $10, $20, $30	; [8] Light Greyscale
.byte $12, $22, $32	; [9] Light Blues
.byte $14, $24, $34	; [A] Light Magentas
.byte $16, $26, $36	; [B] Light Reds
.byte $17, $27, $37	; [C] Light Oranges
.byte $18, $28, $38	; [D] Light Yellows/Pukes
.byte $1A, $2A, $3A	; [E] Light Greens
.byte $1C, $2C, $3C	; [F] Light Cyans
	  
.byte $0F, $0F, $0F	; [10] Black
.byte $20, $10, $00 ; [11] Main Menu Palette
.byte $20, $1C, $3C ; [12] Player Select Palette
.byte $01, $2C, $3C ; [13] Menu Cursor Palette
.byte $0D, $10, $20	; [14] Player Select Background 0

.byte $02, $21, $20 ; [15] Diamond Knight Palette
.byte $05, $27, $20 ; [16] Diamond Knight Alt Palette
