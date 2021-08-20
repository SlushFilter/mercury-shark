; =============================================================================
; particles.asm
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Code for processing active onscreen particles.
; Loaded into Bank3, part of the NS_GAME scope.
; =============================================================================


; F_GetFreeParticle
F_GetFreeParticle:
	ldx #$00
@findLoop:
	lda Par_Type, x
	bpl @foundSlot
	inx
	cpx #$10
	bne @findLoop
	clc
	rts
@foundSlot:
	sec
	rts

; F_SpawnParticle
; v0 = Type of Particle
; v1 = Particle Attribs
; v2 = X position of particle
; v3 = Y position of particle
; v4 = X Velocity, encoded as a 4.4 fixed point value
; v5 = Y Velocity, encoded as a 4.4. fixed point value
; v6 = Particle Argument
F_SpawnParticle:
	jsr F_GetFreeParticle
	bcc @return
	lda v0
	sta Par_Type, x
	lda v1
	sta Par_Attr, x
	lda v2
	sta Par_X, x
	lda v3
	sta Par_Y, x
	lda v4
	sta Par_VX, x
	lda v5
	sta Par_VY, x
	lda v6
	sta Par_Var, x 
@return:
	rts

; =============================================================================
; F_ParticleLogic : EXPORT
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Runs update routines on particles.
; =============================================================================
F_ParticleLogic:
	ldx #$00
@particleLoop:
	lda Par_Type, x						; Check for active particle.
	bpl @nextParticle					; --
	asl									; Fetch the particle function pointer.
	tay                                 ;
	lda FP_ParticleFunctions, y         ;
	sta v0                              ;
	lda FP_ParticleFunctions + $01, y   ; 
	sta v1								; .. and call the particle function.
	jsr _CallParticleFunction			; --
	
@nextParticle:		
	inx									; Index to next Particle.
	cpx #$10                    		;
	bne @particleLoop           		; --
	rts

_CallParticleFunction:		
	jmp(v0)

; #############################################################################
; Particle Function Pointers
; #############################################################################
FP_ParticleFunctions:
.addr Par_Test
.addr Par_SwordSlash

; #############################################################################
; Test Particle																[0]
; #############################################################################
Par_Test:
	lda Par_Tile, x
	cmp #$FB
	beq :+
	lda #$FB
	sta Par_Tile, x
	lda #$04
	sta Par_VX, x
:	lda Par_VX, x
	asl
	asl
	asl
	asl
	clc
	adc Par_SubX, x
	sta Par_SubX, x
	lda #$00
	adc Par_X, x
	sta Par_X, x
	rts

; ##############################################################################
; Sword Slash																 [1]
; ##############################################################################
Par_SwordSlash:
	lda #$23			; Set particle tile.
	sta Par_Tile, x		; --
	
	lda Par_VX, x		; Decode the 4.4 encoded fixed point		
	jsr F_Extend44		; 
	clc					;
	adc Par_SubX, x     ;
	sta Par_SubX, x     ; .. and add it to the X position.
	tya                 ; 
	adc Par_X, x        ;
	sta Par_X, x        ;--
	dec Par_Var, x 		
	bne @return
	lda #$00
	sta Par_Type, x
@return:
	rts 
	
