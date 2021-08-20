; #############################################################################
;  Test Logic													   		   [00]
; #############################################################################
Ent_Test:
	ldy Joy1_State
	tya
	and #JOY_LEFT | JOY_RIGHT	;
	beq @checkVertical			; --
	
	tya
	and #JOY_LEFT
	beq @right
	sec
	lda Ent_X
	sbc #$01
	sta Ent_X
	lda Ent_Screen
	sbc #$00
	sta Ent_Screen
	jmp @hCollide
	
@right:
	clc
	lda #$01
	adc Ent_X
	sta Ent_X
	lda #$00
	adc Ent_Screen
	sta Ent_Screen
	
	
@hCollide:
	ldx Ent_X
	ldy Ent_Y
	lda Ent_Screen
	jsr F_Collide
	
	bcc @checkVertical
	
@checkVertical:
	ldy Joy1_State
	tya
	and #JOY_UP | JOY_DOWN
	beq @drawSprite
	tya
	and #JOY_UP
	beq @down
	lda #$FF
	bne @vMove
@down:	
	lda #$01
@vMove:
	adc Ent_Y
	sta Ent_Y

@drawSprite:

	ldx Ent_X
	ldy Ent_Y
	lda Ent_Screen
	jsr F_Collide
	bcc :+
	
	lda Temp_Y
	sta Ent_Y
	
:	ldy #$03
	jsr F_SetEntSprite
	jsr F_AnimEntSprite
	; jsr F_DrawEnt
	rts
