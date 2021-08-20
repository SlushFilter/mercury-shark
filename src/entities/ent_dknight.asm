
; #############################################################################
;  Diamond Knight												   	
; #############################################################################
.scope ENT_DKNIGHT
.export Ent_DKnight

; Diamond Knight Auxillary Vars . . . . . . . . . . . . . . . . . . . . . . . .
Ent_AtkTimer = Ent_13
Ent_AtkTimer_HI = Ent_14

; Diamond Knight Constants  . . . . . . . . . . . . . . . . . . . . . . . . . . 
DKNIGHT_WALKSPEED = $01
DKNIGHT_WALKSPEED_SUB = $20
DKNIGHT_ACC_RATE = $10
DKNIGHT_FRICTION = $10
DKNIGHT_JUMP_IMPULSE = $04
DKNIGHT_ATK_FRAMES = $10
DKNIGHT_ATK_HITFRAME = $07
DKNIGHT_SATK_FRAMES = $3C
DKNIGHT_SWORDBOX_W	= $0A	; Sword hitbox half-width
DKNIGHT_SWORDBOX_H	= $10	; Sword hitbox full-height
DKNIGHT_SWORDBOX_Y  = $00	; Sword hitbox Y Offset

Ent_DKnight:

	jsr DKnight_Status			; Process Status
	jsr DKnight_Move			; Process Move State
	jsr DKnight_Action			; Process Action State
	jsr DKnight_Animate

	rts
; ============================================================================= 
; Dknight Helper Functions 
; ============================================================================= 

F_SwordHitscan:
	lda Ent_MoveFlags			; Build a left or right facing hitscan box
	and #MOVE_LEFT				;
	bne @leftOfs				;
	clc                         ;
	lda Ent_X                   ;
	adc #DKNIGHT_SWORDBOX_W     ;
	jmp @commitWidth			;
@leftOfs:						;
	sec							;
	lda Ent_X					;
	sbc #DKNIGHT_SWORDBOX_W		;
@commitWidth:	
	sta sx                      ;
	sec                         ;
	lda Ent_Y                   ;
	sbc #DKNIGHT_SWORDBOX_Y     ;
	sta sy                      ;
	lda #DKNIGHT_SWORDBOX_W     ;
	sta sw                      ;
	lda #DKNIGHT_SWORDBOX_H     ;
	sta sh                      ;
	jmp F_BoxEnemyTest          ; and check for hits. (tail return)

F_SpawnSwoosh:
	lda #$81				; Set particle entity #$01 (swoosh)
	sta v0                  ;
	lda Ent_X               ; .. set it for DKnight's current location.
	sta v2                  ;
	lda Ent_Y               ; .. offset -10 pixels from feet.
	sbc #$0C                ;
	sta v3                  ; 
	
	lda Ent_MoveFlags		;
	and #MOVE_LEFT          ;
	bne @leftVel            ;
	lda #$00				; .. no flip attrib.
	sta v1					;
	lda #$28                ; .. set H velocity for 2.5 pixels / frame
	bne @commitVel			;
	
@leftVel:					; Offset for leftward moving swoosh.
	sec						; 
	lda v2					;
	sbc #$08                ;
	sta v2                  ;
	lda #OAM_FLIP_H			; .. set flipped attribute
	sta v1					;
	lda #$E8				;
	
@commitVel:
	sta v4                  ;
	lda #$00                ; .. set V velocity to 0
	sta v5                  ;
	lda #$05                ; .. set duration to 5 frames
	sta v6                  ;
	jmp F_SpawnParticle     ; Spawn the particle (tail return)
	
F_DKnightWalk:	; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 		
	lda Input_State				; Check for Left or Right D-Pad
	and #JOY_LEFT | JOY_RIGHT   ;
	beq @brake					; .. return if neutral.
	
	and #JOY_LEFT				; Check for walking left
	bne @walkLeft               ;
	
	lda Ent_MoveFlags           ; .. handle right press ..
	and #MOVE_LEFT              ;
	bne @groundBrake   			; .. brake if pressing opposite dir of movement
	beq @accelerate             ; .. otherwise accelerate.
	
@walkLeft:						; Handle left press ..
	lda Ent_MoveFlags           ;
	and #MOVE_LEFT              ; 
	beq @groundBrake       		; .. brake if pressing opposite dir of movement
	
@accelerate:
	lda #DKNIGHT_ACC_RATE       ; .. otherwise apply acceleration.
	ldx #DKNIGHT_WALKSPEED      ;
	ldy #DKNIGHT_WALKSPEED_SUB  ;
	jmp F_HAccelerate           ;-- 
	
@groundBrake:
	lda Ent_MoveFlags			; Ground braking when pressing against momentum
	and #MOVE_GROUNDED			;
	beq @brake					;
	lda #DKNIGHT_FRICTION << 1	;
	jsr F_HBrake				; Apply brake accel and return if Vel > 0
	bcs @return					;--
	
	lda Input_State				; Set move direction based on player's input.
	and #JOY_LEFT				;
	beq @setWalkRight           ;--
	
	lda Ent_MoveFlags			; Set Moving Left
	ora #MOVE_LEFT              ;
	sta Ent_MoveFlags           ;
	bne @return                 ;--
	
@setWalkRight:					; Set Moving Right
	lda Ent_MoveFlags           ;
	and #MOVE_LEFT ^ $FF        ;
	sta Ent_MoveFlags           ;
	jmp @return					;--

@brake:							;
	lda #DKNIGHT_FRICTION		; Regular braking, in air or neutral on ground
	jsr F_HBrake				; Apply brake accel and return if Vel > 0

@return:
	rts
	
F_DKnightJump:	; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
	lda #MS_JUMP				; Set Jump MoveState 		
	sta Ent_MoveState           ;
	lda #DKNIGHT_JUMP_IMPULSE	; .. apply jump impulse velocity
	sta Ent_Vel_Y				;
	lda Ent_Vel_X				; .. tweak for X movement velocity
	lsr							;
	lda Ent_Vel_SubX            ;
	ror                         ;
	sta Ent_Vel_SubY            ;
	lda Ent_MoveFlags			;
	ora #MOVE_UP | MOVE_VMOVE	; .. set vertical movement flags.
	and #MOVE_GROUNDED ^ $FF	; .. clear grounded flag.
	sta Ent_MoveFlags			;
	rts							;

; ============================================================================= 
; Status Code
; ============================================================================= 
STATUS_NONE = $00
STATUS_DYING = $01
STATUS_DEAD = $02

DKnight_Status:
	
@return:
	rts
	
; ============================================================================= 
; Movement State Code
; ============================================================================= 

; Diamond Knight Move States  . . . . . . . . . . . . . . . . . . . . . . . . .
MS_GROUND = $00
MS_JUMP   = $01
MS_FALL   = $02

DKnight_Move:
	lda Ent_MoveState			; Jump to the current movement state logic
	asl							;
	tay 						;
	lda FP_MoveStates, y        ;
	sta v0                      ;
	lda FP_MoveStates + 1, y    ;
	sta v1                      ;
	jmp (v0)                    ; --
	
FP_MoveStates:
	.addr MoveState_Stand
	.addr MoveState_Jump
	.addr MoveState_Fall
	.addr MoveState_Recoil
	
MoveState_Stand:	; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
; Check for L/R and Jump inputs, switch state if needed.
; Check ground every frame and fall if the ground is gone.
; Otherwise apply friction until stopped. 
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
	lda Input_State				; Check for an A press on this frame.
	and #JOY_A                  ;
	and Input_Delta           	;
	beq @move					;
	
	jsr F_DKnightJump			; Apply jump impulse and immediatly switch to 
	jmp MoveState_Jump			; .. the jumping state.
	
@move:	
	jsr F_DKnightWalk			; .. process a walk move
	lda Ent_Vel_X				;
	ora Ent_Vel_SubX            ;
	beq @checkGround            ; .. skip left/right move if stopped
	jsr F_HMove					; .. apply left or right movement.
	
@checkGround:					; Check for ground underneath us.
	jsr F_CheckGround			; 
	bcs @return					; 
	lda #MS_FALL				; Set falling state if there is no ground.
	sta Ent_MoveState			; 
@return:	
	rts
	
MoveState_Jump:	; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Apply left or right acceleration depending on DPad
; Check vertical velocity, switch to fall state if moving down.
; Check for holding an A press and cut vertical velocity if released.
; Apply horizontal movement
; Apply vertical movement
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
	jsr F_DKnightWalk
	jsr F_HMove
	lda Input_State
	and #JOY_A
	bne @applyGravity
	clc
	lda Ent_Vel_Y
	lsr
	sta Ent_Vel_Y
	lda Ent_Vel_SubY
	ror
	sta Ent_Vel_SubY
@applyGravity:
	jsr F_ApplyGravity
	jsr F_VMove
	lda Ent_MoveFlags
	and #MOVE_UP
	bne @return
	lda #MS_FALL
	sta Ent_MoveState
@return:
	rts
	
MoveState_Fall:	; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Apply airwalk based on D-Pad input
; Apply horizontal movement
; Apply gravity
; Apply vertical movement
; Check for grounded and update state if needed.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
	jsr F_DKnightWalk

MoveState_Recoil: ; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Same as MoveState_Fall, but disable airwalk.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
	jsr F_HMove
	jsr F_ApplyGravity
	jsr F_VMove
	bcc @return
	lda Ent_MoveFlags
	and #MOVE_GROUNDED
	beq @return
	lda #MS_GROUND
	sta Ent_MoveState
@return:
	rts

; ============================================================================= 
; Action State Code
; ============================================================================= 

; Diamond Knight Action States  . . . . . . . . . . . . . . . . . . . . . . . .
AS_NONE = $00
AS_ATTACK = $01
AS_SATTACK = $02

; Attack Constants  . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

DKnight_Action:
	lda Ent_ActionState
	asl
	tay
	lda FP_ActionStates, y
	sta $00
	lda FP_ActionStates + 1, y
	sta $01
	jmp ($00)
	
FP_ActionStates:
	.addr ActionState_None
	.addr ActionState_Attack
	.addr ActionState_SAttack

ActionState_None:		; . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Check for B button press
; Check for Select button press
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
	lda Input_State		; Check for standard attack	
	tay                 ;
	and Input_Delta		;
	and #JOY_B          ; .. branch if player wants to attack.
	bne _enterAttack	;--
	
	tya                 ; Check for special attack
	and #JOY_SELECT     ;
	bne _enterSAttack	; .. branch if player wants to special attack.
	rts                 ;--

_enterAttack:
	lda #AS_ATTACK				; Put DKnight into attack state.
	sta Ent_ActionState         ;
	lda #DKNIGHT_ATK_FRAMES     ;
	sta Ent_AtkTimer            ; --
	
	jsr F_SpawnSwoosh			; Spawn the swoosh particle
	jsr F_SwordHitscan			; Perform the sword hitscan
	
ActionState_Attack:		; . . . . . . . . . . . . . . . . . . . . . . . . . . .
	dec Ent_AtkTimer
	bne @return
	lda #AS_NONE
	sta Ent_ActionState
@return:
	rts

_enterSAttack:	
	lda #AS_SATTACK
	sta Ent_ActionState
	lda #DKNIGHT_SATK_FRAMES
	sta Ent_AtkTimer
	
ActionState_SAttack:	; . . . . . . . . . . . . . . . . . . . . . . . . . . .
	dec Ent_AtkTimer
	bne @return
	lda #AS_NONE
	sta Ent_ActionState
@return:
	rts

; ============================================================================= 
; Animation Code
; ============================================================================= 

; Diamond Knight Animations . . . . . . . . . . . . . . . . . . . . . . . . . . 
DKNIGHT_IDLE = $00
DKNIGHT_WALK = $01
DKNIGHT_JUMP = $02
DKNIGHT_ATTACK_RIGHT = $06
DKNIGHT_ATTACK_LEFT = $07


DKnight_Animate:
	lda Ent_ActionState
	beq @walkAnims

	lda Ent_FrameMod
	and #OAM_FLIP_H ^ $FF
	sta Ent_FrameMod
	ldy #DKNIGHT_ATTACK_RIGHT
	lda Ent_MoveFlags
	and #MOVE_LEFT
	beq @animate
	ldy #DKNIGHT_ATTACK_LEFT
	bne @animate

@walkAnims:
	ldy #DKNIGHT_WALK
	lda Ent_MoveFlags
	and #MOVE_LEFT
	beq @setWalkRight

	lda Ent_FrameMod
	ora #OAM_FLIP_H
	sta Ent_FrameMod
	bne @animate
	
@setWalkRight:
	lda Ent_FrameMod
	and #OAM_FLIP_H ^ $FF
	sta Ent_FrameMod

@animate:
	jsr F_SetEntSprite
	jsr F_AnimEntSprite
	rts
	
.endscope



































