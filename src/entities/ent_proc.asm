; =============================================================================
; particles.asm
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Code for processing active entities, logic for entities is kept in separate 
; files.
;
; Loaded into Bank3, part of the NS_GAME scope.
; =============================================================================


; =============================================================================
; F_EntityLogic : EXPORT
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Cycles through all entities and processes them each if they are active.
; =============================================================================
F_EntityLogic:
		lda #$00		; Set the current entity slot
		sta Ent_Idx     ;--
@procEnt:
		ldx Ent_Idx		; Grab current entity process index.
		lda Ent_Data, x	; Check entity for activation flag.
		bpl @nextEnt	;--
		
		jsr F_StageEnt	; Stage the entity for processing.
		
		jsr F_UpdateEnt ; Update the Entity
		
		ldx Ent_Idx		; Commit the update.
		jsr F_CommitEnt ;--
		
@nextEnt:
		clc				;
		lda #$20        ;
		adc Ent_Idx		;
		sta Ent_Idx     ;
		bne @procEnt    ;--
		
		rts
		
; =============================================================================
; F_StageEnt
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Stages an entity for processing.
; X - Entity slot to stage.
; =============================================================================
F_StageEnt:
	lda Ent_Data, x			; Unrolled loop to copy EntSlot -> Ent for 
	sta Ent                 ; .. processing.
	lda Ent_Data + $01, x   ;
	sta Ent + $01           ;
	lda Ent_Data + $02, x   ;
	sta Ent + $02           ;
	lda Ent_Data + $03, x   ;
	sta Ent + $03           ;
	lda Ent_Data + $04, x   ;
	sta Ent + $04           ;
	lda Ent_Data + $05, x   ;
	sta Ent + $05           ;
	lda Ent_Data + $06, x   ;
	sta Ent + $06           ;
	lda Ent_Data + $07, x   ;
	sta Ent + $07           ;
	lda Ent_Data + $08, x   ;
	sta Ent + $08           ;
	lda Ent_Data + $09, x   ;
	sta Ent + $09           ;
	lda Ent_Data + $0A, x   ;
	sta Ent + $0A           ;
	lda Ent_Data + $0B, x   ;
	sta Ent + $0B           ;
	lda Ent_Data + $0C, x   ;
	sta Ent + $0C           ;
	lda Ent_Data + $0D, x   ;
	sta Ent + $0D           ;
	lda Ent_Data + $0E, x   ;
	sta Ent + $0E           ;
	lda Ent_Data + $0F, x   ;
	sta Ent + $0F           ;
	lda Ent_Data + $10, x   ;
	sta Ent + $10           ;
	lda Ent_Data + $11, x   ;
	sta Ent + $11           ;
	lda Ent_Data + $12, x   ;
	sta Ent + $12           ;
	lda Ent_Data + $13, x   ;
	sta Ent + $13           ;
	lda Ent_Data + $14, x   ;
	sta Ent + $14           ;
	lda Ent_Data + $15, x   ;
	sta Ent + $15           ;
	lda Ent_Data + $16, x   ;
	sta Ent + $16           ;
	lda Ent_Data + $17, x   ;
	sta Ent + $17           ;
	lda Ent_Data + $18, x   ;
	sta Ent + $18           ;
	lda Ent_Data + $19, x   ;
	sta Ent + $19           ;
	lda Ent_Data + $1A, x   ;
	sta Ent + $1A           ;
	lda Ent_Data + $1B, x   ;
	sta Ent + $1B           ;
	lda Ent_Data + $1C, x   ;
	sta Ent + $1C           ;
	lda Ent_Data + $1D, x   ;
	sta Ent + $1D           ;
	lda Ent_Data + $1E, x   ;
	sta Ent + $1E           ;
	lda Ent_Data + $1F, x   ;
	sta Ent + $1F           ;--
	rts

; =============================================================================
; F_CommitEnt
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Commits an entity after processing.
;
; X - The entity slot to commit to.
; =============================================================================
F_CommitEnt:
	lda Ent					; Unrolled loop to copy Ent -> EntSlot to commit
	sta Ent_Data, x			; .. processing.
	lda Ent + $01           ;
	sta Ent_Data + $01, x   ;
	lda Ent + $02           ;
	sta Ent_Data + $02, x   ;
	lda Ent + $03           ;
	sta Ent_Data + $03, x   ;
	lda Ent + $04           ;
	sta Ent_Data + $04, x   ;
	lda Ent + $05           ;
	sta Ent_Data + $05, x   ;
	lda Ent + $06           ;
	sta Ent_Data + $06, x   ;
	lda Ent + $07           ;
	sta Ent_Data + $07, x   ;
	lda Ent + $08           ;
	sta Ent_Data + $08, x   ;
	lda Ent + $09           ;
	sta Ent_Data + $09, x   ;
	lda Ent + $0A           ;
	sta Ent_Data + $0A, x   ;
	lda Ent + $0B           ;
	sta Ent_Data + $0B, x   ;
	lda Ent + $0C           ;
	sta Ent_Data + $0C, x   ;
	lda Ent + $0D           ;
	sta Ent_Data + $0D, x   ;
	lda Ent + $0E           ;
	sta Ent_Data + $0E, x   ;
	lda Ent + $0F           ;
	sta Ent_Data + $0F, x   ;
	lda Ent + $10           ;
	sta Ent_Data + $10, x   ;
	lda Ent + $11           ;
	sta Ent_Data + $11, x   ;
	lda Ent + $12           ;
	sta Ent_Data + $12, x   ;
	lda Ent + $13           ;
	sta Ent_Data + $13, x   ;
	lda Ent + $14           ;
	sta Ent_Data + $14, x   ;
	lda Ent + $15           ;
	sta Ent_Data + $15, x   ;
	lda Ent + $16           ;
	sta Ent_Data + $16, x   ;
	lda Ent + $17           ;
	sta Ent_Data + $17, x   ;
	lda Ent + $18           ;
	sta Ent_Data + $18, x   ;
	lda Ent + $19           ;
	sta Ent_Data + $19, x   ;
	lda Ent + $1A           ;
	sta Ent_Data + $1A, x   ;
	lda Ent + $1B           ;
	sta Ent_Data + $1B, x   ;
	lda Ent + $1C           ;
	sta Ent_Data + $1C, x   ;
	lda Ent + $1D           ;
	sta Ent_Data + $1D, x   ;
	lda Ent + $1E           ;
	sta Ent_Data + $1E, x   ;
	lda Ent + $1F           ;
	sta Ent_Data + $1F, x   ;--
	
	rts

; =============================================================================
; F_UpdateEnt
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Runs an entity's update routine.
; =============================================================================
F_UpdateEnt:
	lda Ent_Type						; Call the entity process function for
	asl									; .. the currently processing entity.
	tax                                 ;
	lda FP_EntityFunctions, x           ;
	sta v0                              ;
	lda FP_EntityFunctions + $01, x     ;
	sta v1                              ;
	jmp (v0)                            ;--