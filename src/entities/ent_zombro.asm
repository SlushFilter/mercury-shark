; #############################################################################
;  Zombro													   		   	   [00]
; #############################################################################

ZOMBRO_WIDTH = $04		; Hitbox half-width
ZOMBRO_HEIGHT = $10		; Hitbox height
ZOMBRO_WALKSPEED = $20
ANIM_ZOMBROWALK_RIGHT = $04
ANIM_ZOMBROWALK_LEFT = $05

; AI Stage 0 - Spawn
; AI Stage 1 - Walk 
; AI Stage 2 - Dying ?
; AI Stage 3 - Dead / Despawn

Ent_Zombro:
	lda Ent_Damaged
	beq @aiStage
	lda #$00
	sta Ent_Damaged
	lda #$02
	sta Ent_AiStage

@aiStage:
	lda Ent_AiStage				; Select Zombro's think function based on the 
	asl                         ; .. current AI Stage, then jump to that
	tay                         ; .. function.
	lda FP_ZombroThink, y       ;
	sta v0                      ;
	lda FP_ZombroThink + 1, y   ;
	sta v1                      ;
	jmp (v0)                    ; --
	
FP_ZombroThink:
	.addr Ent_ZombroSpawn	; [0] Spawn
	.addr Ent_ZombroPatrol  ; [1] Patrol Left & Right
	.addr Ent_ZombroDie     ; [2] Death Sequence
	.addr Ent_ZombroDead    ; [3] Despawn

; ZombroSpawn - Select a Random Direction
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ent_ZombroSpawn:
	lda Rng						; Randomly select left or right movement.
	and #$01					;
	beq @moveRight				;
	lda Ent_MoveFlags			;
	ora #MOVE_LEFT   			;
	bne @commitMove   			;
@moveRight:						;
	lda Ent_MoveFlags			;
	and #MOVE_LEFT ^ $FF     	;
@commitMove:                  	;
	sta Ent_MoveFlags           ; --
	
	lda #ZOMBRO_WALKSPEED		; Set initial velocity.
	sta Ent_Vel_SubX			; --
	
	ldy #ANIM_ZOMBROWALK_RIGHT	; Set initial animation.
	jsr F_SetEntSprite			; --
	
	lda #$01						; Next AI Stage = #$01
	sta Ent_AiStage				; --
	rts
	
; ZombroPatrol - Walk in a direction until you wall-bonk.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ent_ZombroPatrol:
	lda Ent_MoveFlags			; Check for walking left or right.
	bmi @walkLeft				; --
	
@walkRight:
	jsr F_MoveRightSolid		; Perform a RightMove with collision.
	bcs @turnAround				; .. turn around on a wall bonk.
	bcc @checkGround
	
@walkLeft:
	jsr F_MoveLeftSolid			; Perform a LeftMove with collision.
	bcs @turnAround				; .. 

@checkGround:
	jsr F_CheckGround
	bcs @bodyCheck
	
@turnAround:
	lda Ent_MoveFlags			; Flip the walk-direction flag.
	eor #MOVE_LEFT              ;
	sta Ent_MoveFlags           ; --
	
	lda #ZOMBRO_WALKSPEED		; .. reset walking velocity.
	sta Ent_Vel_SubX	        ; 
	
@bodyCheck:
	ldy #$01
	jsr F_EnemyPlayerTest
	
@drawZombro:
	ldy #ANIM_ZOMBROWALK_RIGHT	; Default right animation
	lda Ent_MoveFlags			; .. check if moving left.
	beq @setAnim
	iny							; .. set left animation if needed.
@setAnim:
	jsr F_SetEntSprite
	jsr F_AnimEntSprite			; Animate Zombro's walk cycle.
	rts
	
; ZombroDie - Zombro is currently dying.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ent_ZombroDie:
	lda Ent_FrameMod
	eor #$03
	sta Ent_FrameMod
	sec
	lda Ent_Y
	sbc #$01
	sta Ent_Y
	bcs @return
	lda #$03
	sta Ent_AiStage
@return:
	rts
	
; ZombroDead - Zombro is dead, despawn it.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
Ent_ZombroDead:
	lda #$00
	sta Ent_Flags + $00
	sta Ent_Flags + $01
	sta Ent_Flags + $02
	sta Ent_Flags + $03
	sta Ent_Flags + $04
	sta Ent_Flags + $05
	sta Ent_Flags + $06
	sta Ent_Flags + $07
	sta Ent_Flags + $08
	sta Ent_Flags + $09
	sta Ent_Flags + $0A
	sta Ent_Flags + $0B
	sta Ent_Flags + $0C
	sta Ent_Flags + $0D
	sta Ent_Flags + $0E
	sta Ent_Flags + $0F
	sta Ent_Flags + $10
	sta Ent_Flags + $11
	sta Ent_Flags + $12
	sta Ent_Flags + $13
	sta Ent_Flags + $14
	sta Ent_Flags + $15
	sta Ent_Flags + $16
	sta Ent_Flags + $17
	sta Ent_Flags + $18
	sta Ent_Flags + $19
	sta Ent_Flags + $1A
	sta Ent_Flags + $1B
	sta Ent_Flags + $1C
	sta Ent_Flags + $1D
	sta Ent_Flags + $1E
	sta Ent_Flags + $1F
	rts