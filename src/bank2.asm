; =============================================================================
; Bank 2 - Tile CHR Data
; =============================================================================

.export DP_TileSets
.define BANK_NUMBER		$02		; Bank Identifier

.segment "BANK2"
.org $8000
.byte BANK_NUMBER

; =============================================================================
; DP_TileSets :
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Pointers to tile chr data.
; =============================================================================
DP_TileSets:
.addr CHR_FONT		; [00]
.addr CHR_MENUBG    ; [01]
.addr CHR_STAGE00   ; [02]

CHR_FONT:
.incbin "chr-data/font.chr"
CHR_MENUBG:
.incbin "chr-data/background.chr"
CHR_STAGE00:
.incbin "chr-data/stage00.chr"
