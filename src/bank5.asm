; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Bank 5 - Sprite Animations
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.include "defines.inc"
.include "ram-map.inc"
.define BANK_NUMBER		$05		; Bank Identifier
.segment "BANK5"
.org $8000
.byte BANK_NUMBER

.scope NS_SPRITES
.export D_AnimPtrs, F_DrawSprites, F_DrawParticles

; Byte offsets for source data.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
SCREEN 		= Ent_Screen	  - $20
LOCX		= Ent_X			  - $20
LOCY		= Ent_Y           - $20
CHRBASE		= Ent_ChrBase	  -	$20 
ANIMINDEX	= Ent_AnimIndex	  - $20
FRAMECOUNT	= Ent_FrameCount  - $20
FRAMETIMER  = Ent_FrameTimer  - $20
FRAMEMOD	= Ent_FrameMod	  - $20
FRAMEPTR	= Ent_FramePtr	  - $20
FRAMEPTR_LO	= Ent_FramePtr_LO - $20
FRAMEPTR_HI	= Ent_FramePtr_HI - $20

; =============================================================================
; F_DrawSprites :
; =============================================================================
F_DrawSprites:
	ldx #$00						; Iterate through all Entities
@entLoop:
	
	lda Ent_Data, x					; Loop if entity is inactive.
	bpl @nextEnt	               	; --

	
	lda Ent_Data + FRAMEPTR_LO, x	; Fetch the relevant entity data.
	sta v0             				; 
	lda Ent_Data + FRAMEPTR_HI, x   ; (word)v0 = FramePtr
	sta v1             				;
	lda Ent_Data + CHRBASE, x		;
	sta Ent_ChrBase					;
	lda Ent_Data + FRAMEMOD, x		;
	sta Ent_FrameMod				;
	lda Ent_Data + SCREEN, x		;
	sta Ent_Screen                  ;
	lda Ent_Data + LOCX, x			;
	sta Ent_X                       ;
	lda Ent_Data + LOCY, x          ;
	sta Ent_Y                     	; --
	
	ldy #$00						; Fetch the Entity Frame Pointer
	lda (v0), y						; v2 = FrameCount
	iny								;
	sta v2							; --
	
	stx v3							; Entity Index
	ldx OamIndex					; Fetch OamIndexer
	
@spriteLoop:
	
	clc								; Sprite_Y
	lda (v0), y						;
	iny								;
	adc Ent_Y						;
	sta OamRam, x					;
	inx 							; --

	clc								; Sprite_Index
	lda (v0), y						;
	iny								;
	adc Ent_ChrBase					;
	sta OamRam, x					;
	inx								; --
	
	lda (v0), y						; Sprite_Attrib
	iny								;
	eor Ent_FrameMod				;
	sta OamRam, x                   ;
	inx                             ; --

	clc								; Sprite_X
	lda (v0), y                     ; Ofs_X + Write Ent_X - Scroll_X
	iny								;
	adc Ent_X                       ;
	sbc Scroll_X                    ;
	sta OamRam, x                   ;
	inx                             ; --
	
	dec v2							; Check for end of sprites
	bne @spriteLoop					; --
	
	stx OamIndex
	ldx v3
	
@nextEnt:
	txa								; Step to the next entity for processing.
	clc 							;
	adc #$20                        ;
	tax								;
	bne @entLoop                    ; --

@return:
	rts

; =============================================================================
; F_DrawParticles :
; =============================================================================
F_DrawParticles:
	ldx OamIndex
	ldy #$00
@parLoop:	
	lda Par_Type, y
	bpl @nextPar
	
	lda Par_Y, y            ; Par_Y
	sta OamRam, x           ;
	inx                     ; --
	
	lda Par_Tile, y			; Par_Tile
	sta OamRam, x           ;
	inx                     ; --
	
	lda Par_Attr, y			; Par_Attr
	sta OamRam, x           ;
	inx                     ; --
	
	sec						; Par_X
	lda Par_X, y            ;
	sbc Scroll_X            ;
	sta OamRam, x           ;
	inx                     ; --
	
	cpx #$00				; Exit if OamRam rolled over.
	beq @return				; --
	
@nextPar:
	iny						; Next Particle
	cpy #$10                ;
	bcc @parLoop            ; --

@clearSprites:	
	lda #$F4
@clearLoop:
	sta OamRam, x           ; Move unused sprites offscreen.
	inx                     ;
	inx                     ;
	inx                     ;
	inx                     ;
	bne @clearLoop          ; --
@return:
	stx OamIndex
	rts
	
; =============================================================================
; D_AnimPtrs :
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Animation data pointers.
; Byte	Name
; 0:1	PtrFrameData	Pointer to the start of frame Data
; 2		AnimLength
; 3		FrameDuration
; =============================================================================
D_AnimPtrs:
ANIMPTR DKnight_Walk, 		$01, $FF		; [00] DKnight Idle animation
ANIMPTR DKnight_Walk,		$02, $FF		; [01] DKnight Walk animation
ANIMPTR DKnight_Jump,		$01, $FF		; [02] DKnight Jump animation
ANIMPTR Coin_Spin,			$0A, $18		; [03] Coin_Spin
ANIMPTR Zombro_WalkRight,	$04, $FF		; [04] Zombro Walk Right
ANIMPTR Zombro_WalkLeft,	$04, $FF		; [05] Zombro Walk Left 
ANIMPTR DKnight_AtkRight,   $06, $11		; [06] DKnight Attack Right
ANIMPTR DKnight_AtkLeft,    $06, $11		; [07] DKnight Attack Left

; =============================================================================
; D_FrameData :
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Animation frame data. Frames are stored sequentially.
;
; 0		SpriteCount	Number of sprites for this frame.
; 1 	Yoffset		
; 2		TileIndex
; 3		Attribs
; 4		Xoffset
; =============================================================================
DKnight_Walk:
.byte $02, 	$EF, $00, $00, $FC,		$F7, $10, $00, $FC
DKnight_Jump:
.byte $02,  $F0, $00, $00, $FC,		$F8, $11, $00, $FC

Coin_Spin:
.byte $01,	$00, $FB, $00, $00
.byte $01,	$00, $FB, $00, $00
.byte $01,	$00, $FB, $00, $00
.byte $01,	$00, $FC, $00, $00
.byte $01,	$00, $FC, $00, $00
.byte $01,	$00, $FD, $00, $00
.byte $01,	$00, $FE, $00, $00
.byte $01,	$00, $FD, $40, $00
.byte $01,	$00, $FC, $00, $00
.byte $01,	$00, $FC, $00, $00

Zombro_WalkRight:
;			head				 Body					Legs
.byte $03,  $ED, $80, $00, $FD,	 $F2, $81, $00, $FE,	$F8, $82, $00, $FC
.byte $03,  $ED, $80, $00, $FE,	 $F2, $81, $00, $FF,	$F8, $83, $00, $FC
.byte $03,  $EE, $80, $00, $FE,	 $F3, $81, $00, $FF,	$F8, $84, $00, $FC
.byte $03,  $EE, $80, $00, $FE,	 $F3, $81, $00, $FE,	$F8, $83, $00, $FC

Zombro_WalkLeft:
;			head				 Body					Legs
.byte $03,  $ED, $80, $40, $FD,	 $F2, $81, $40, $FB,	$F8, $82, $40, $FD
.byte $03,  $ED, $80, $40, $FC,	 $F2, $81, $40, $FA,	$F8, $83, $40, $FD
.byte $03,  $EE, $80, $40, $FC,	 $F3, $81, $40, $FA,	$F8, $84, $40, $FD
.byte $03,  $EE, $80, $40, $FC,	 $F3, $81, $40, $FB,	$F8, $83, $40, $FD

DKnight_AtkRight:
	.byte $02
	FRAMEDATA - 4,-16,$02, 0
	FRAMEDATA - 4,- 8,$12, 0
DKnight_AtkRight_1_data:
	.byte $02
	FRAMEDATA - 4,-16,$02,0
	FRAMEDATA - 4,- 8,$12,0
DKnight_AtkRight_2_data:
	.byte $04
	FRAMEDATA - 3,-16,$02,0
	FRAMEDATA - 4,- 8,$13,0
	FRAMEDATA   4,-12,$20,0
	FRAMEDATA  12,-12,$21,0

DKnight_AtkRight_3_data:
	.byte $04
	FRAMEDATA - 3,-16,$02,0
	FRAMEDATA - 4,- 8,$13,0
	FRAMEDATA   4,-12,$22,0
	FRAMEDATA  12,-12,$23,0

DKnight_AtkRight_4_data:
	.byte $04
	FRAMEDATA - 4,-16,$02,0
	FRAMEDATA - 4,- 8,$12,0
	FRAMEDATA   4,-12,$24,0
	FRAMEDATA  12,-12,$25,0

DKnight_AtkRight_5_data:
	.byte $02
	FRAMEDATA - 4,-16,$02,0
	FRAMEDATA - 4,- 8,$12,0
	
DKnight_AtkLeft:
	.byte $02
	FRAMEDATA - 4,-16,$02, OAM_FLIP_H
	FRAMEDATA - 4,- 8,$12, OAM_FLIP_H

DKnight_AtkLeft_1_data:
	.byte $02
	FRAMEDATA - 4,-16,$02, OAM_FLIP_H
	FRAMEDATA - 4,- 8,$12, OAM_FLIP_H

DKnight_AtkLeft_2_data:
	.byte $04
	FRAMEDATA - 5,-16,$02, OAM_FLIP_H
	FRAMEDATA - 4,- 8,$13, OAM_FLIP_H
	FRAMEDATA -12,-12,$20, OAM_FLIP_H
	FRAMEDATA -20,-12,$21, OAM_FLIP_H

DKnight_AtkLeft_3_data:
	.byte $04
	FRAMEDATA - 5,-16,$02, OAM_FLIP_H
	FRAMEDATA - 4,- 8,$13, OAM_FLIP_H
	FRAMEDATA -12,-12,$22, OAM_FLIP_H
	FRAMEDATA -20,-12,$23, OAM_FLIP_H

DKnight_AtkLeft_4_data:
	.byte $04
	FRAMEDATA - 4,-16,$02, OAM_FLIP_H
	FRAMEDATA - 4,- 8,$12, OAM_FLIP_H
	FRAMEDATA -12,-12,$24, OAM_FLIP_H
	FRAMEDATA -20,-12,$25, OAM_FLIP_H

DKnight_AtkLeft_5_data:
	.byte $02
	FRAMEDATA - 4,-16,$02, OAM_FLIP_H
	FRAMEDATA - 4,- 8,$12, OAM_FLIP_H




.endscope