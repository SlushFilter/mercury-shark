; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Bank 0 - Famitone5 Driver, BGM and SFX
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.export  F_FamitoneInit, F_FamitoneUpdate, song_music_data, sounds

.define BANK_NUMBER		$00		; Bank Identifier
.segment "BANK0"
.org $8000

.byte BANK_NUMBER
.include "famitone5.asm"
.include "defines.inc"
.include "ram-map.inc"

; =============================================================================
; F_FamiToneInit
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Initalize the Famitone 5 Driver
; =============================================================================
DMARKER "### FamiTone Init ###"
F_FamitoneInit:
	ldx #<song_music_data	; Initialize Music Data
	ldy #>song_music_data   ;
	jsr FamiToneInit        ; --
	ldx #<sounds			; Initialize Sfx Data
	ldy #>sounds   			;
	jsr FamiToneSfxInit     ; --

	lda #$FF				; Clear all pending SFX's
	sta SFX_0               ;
	sta SFX_1               ;
	sta SFX_2               ;
	sta SFX_3               ;--
	
	rts
; =============================================================================
; F_FamitoneUpdate
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Fires off any pending sound effects and processes one frame of audio.
; =============================================================================
DMARKER "### FamiTone Update ###"
F_FamitoneUpdate:
	lda SFX_0				; Play any pending SFX in Channel 0
	cmp #$FF				;
	beq :+                  ;
	ldx #FT_SFX_CH0 		;
	jsr FamiToneSfxPlay     ;--

:	lda SFX_1				; Play any pending SFX in Channel 1
	cmp #$FF                ;
	beq :+                  ;
	ldx #FT_SFX_CH1         ;
	jsr FamiToneSfxPlay     ;--

:	lda SFX_2				; Play any pending SFX in Channel 2
	cmp #$FF                ;
	beq :+                  ;
	ldx #FT_SFX_CH2         ;
	jsr FamiToneSfxPlay     ;--

:	lda SFX_3				; Play any pending SFX in Channel 3
	cmp #$FF                ;
	beq :+                  ;
	ldx #FT_SFX_CH3         ;
	jsr FamiToneSfxPlay     ;--

:	lda #$FF				; Clear all pending SFX's
	sta SFX_0               ;
	sta SFX_1               ;
	sta SFX_2               ;
	sta SFX_3               ;--
	
	lda BGM					; Skip song #$00
	beq @update				;--
	
	cmp #$FF				; Stop current song.
	beq @stopMusic			;--
	
	jsr FamiToneMusicPlay	; Play selected Bgm
	jmp @update				;--
	
@stopMusic:
	jsr FamiToneMusicStop	; Stop the music!
	
@update:
	lda #$00				; #$00 flags no futher song changing action.
	sta BGM					;--
	jsr FamiToneUpdate      ; Update FamiTone
	rts
	
; Music Dataset
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DMARKER "### FamiTone BGM Data ###"
song_music_data:
	.byte 1
	.word @instruments
	.word @samples-3
	.word @song0ch0,@song0ch1,@song0ch2,@song0ch3,@song0ch4,307,256 ; New song

@instruments:
	.word @env1,@env0,@env0,@env0

@samples:
@env0:
	.byte $c0,$00,$00
@env1:
	.byte $c0,$cf,$cb,$ca,$c9,$c8,$00,$05


; New song
@song0ch0:
	.byte $fb,$06
@song0ch0loop:
@ref0:
	.byte $80,$1c,$85,$1c,$85,$28,$85,$1c,$85,$1c,$85,$19,$85,$1a,$85,$1b
	.byte $85,$1c,$85,$1c,$89,$1c,$85,$10,$81,$1c,$85,$1c,$89,$1c,$85,$1c
	.byte $81
	.byte $fd
	.word @song0ch0loop

; New song
@song0ch1:
@song0ch1loop:
@ref1:
	.byte $87,$80,$34,$8d,$2f,$8d,$2d,$8d,$2e,$8d,$2f,$89,$2f,$85,$2d,$89
	.byte $2c,$8d,$2f,$85
	.byte $fd
	.word @song0ch1loop

; New song
@song0ch2:
@song0ch2loop:
@ref2:
	.byte $f9,$85
	.byte $fd
	.word @song0ch2loop

; New song
@song0ch3:
@song0ch3loop:
@ref3:
	.byte $f9,$85
	.byte $fd
	.word @song0ch3loop

; New song
@song0ch4:
@song0ch4loop:
@ref4:
	.byte $f9,$85
	.byte $fd
	.word @song0ch4loop

; Sfx Dataset
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
DMARKER "### FamiTone SFX Data ###"
sounds:
	.word @ntsc
	.word @pal
@ntsc:
	.word @sfx_ntsc_0
	.word @sfx_ntsc_1
@pal:
	.word @sfx_pal_0
	.word @sfx_pal_1

@sfx_ntsc_0:
	.byte $80,$7e,$81,$2b,$82,$01,$01,$80,$bb,$81,$fb,$82,$00,$01,$80,$30
	.byte $01,$80,$b7,$81,$9b,$01,$80,$b5,$81,$6b,$01,$00
@sfx_pal_0:
	.byte $80,$7e,$81,$1c,$82,$01,$01,$80,$bb,$81,$ec,$82,$00,$01,$80,$30
	.byte $01,$80,$b7,$81,$8c,$01,$80,$b5,$81,$5c,$00
@sfx_ntsc_1:
	.byte $83,$bf,$84,$d5,$85,$00,$01,$83,$ba,$84,$d7,$01,$83,$b7,$84,$d5
	.byte $01,$83,$b5,$01,$83,$be,$84,$9f,$01,$83,$b9,$84,$a1,$01,$83,$b6
	.byte $84,$9f,$01,$83,$b4,$01,$83,$bd,$84,$8e,$01,$83,$b8,$84,$90,$01
	.byte $83,$b6,$84,$8e,$01,$83,$b4,$01,$83,$b2,$01,$83,$b1,$02,$83,$b8
	.byte $84,$d5,$01,$83,$b5,$84,$d7,$01,$83,$b3,$84,$d5,$01,$83,$b7,$84
	.byte $9f,$01,$83,$b4,$84,$a1,$01,$83,$b3,$84,$9f,$01,$83,$b2,$01,$83
	.byte $b6,$84,$8e,$01,$83,$b4,$84,$90,$01,$83,$b2,$84,$8e,$02,$83,$b1
	.byte $03,$83,$b4,$84,$d5,$01,$83,$b2,$84,$d7,$01,$83,$b1,$84,$d5,$01
	.byte $83,$b3,$84,$9f,$01,$83,$b2,$84,$a1,$01,$83,$b1,$84,$9f,$02,$83
	.byte $b2,$84,$8e,$01,$83,$b1,$84,$90,$01,$84,$8e,$05,$83,$b2,$84,$d5
	.byte $01,$83,$b1,$84,$d7,$01,$84,$d5,$01,$84,$9f,$01,$84,$a1,$01,$84
	.byte $9f,$02,$00
@sfx_pal_1:
	.byte $83,$bf,$84,$c6,$85,$00,$01,$83,$ba,$84,$c8,$01,$83,$b7,$84,$c6
	.byte $01,$83,$be,$84,$94,$01,$83,$b9,$84,$96,$01,$83,$b6,$84,$94,$01
	.byte $83,$bd,$84,$84,$01,$83,$b8,$84,$86,$01,$83,$b6,$84,$84,$01,$83
	.byte $b4,$01,$83,$b2,$01,$83,$b1,$01,$83,$b8,$84,$c6,$01,$83,$b5,$84
	.byte $c8,$01,$83,$b3,$84,$c6,$01,$83,$b7,$84,$94,$01,$83,$b4,$84,$96
	.byte $01,$83,$b3,$84,$94,$01,$83,$b6,$84,$84,$01,$83,$b4,$84,$86,$01
	.byte $83,$b2,$84,$84,$02,$83,$b1,$02,$83,$b4,$84,$c6,$01,$83,$b2,$84
	.byte $c8,$01,$83,$b1,$84,$c6,$01,$83,$b3,$84,$94,$01,$83,$b2,$84,$96
	.byte $01,$83,$b1,$84,$94,$01,$83,$b2,$84,$84,$01,$83,$b1,$84,$86,$01
	.byte $84,$84,$04,$83,$b2,$84,$c6,$01,$83,$b1,$84,$c8,$01,$84,$c6,$01
	.byte $84,$94,$01,$84,$96,$01,$84,$94,$01,$00

