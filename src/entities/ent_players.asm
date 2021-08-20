; =============================================================================
; ent_players.asm
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Code for the player character controllers, these select a set of player
; entity logic to execute on based on the player's selection at the beginning of
; the game.
;
; Loaded into Bank3, part of the NS_GAME scope.
; =============================================================================

; #############################################################################
;  Player 1												   		   		   [01]
; #############################################################################
Ent_Player1:
	
	lda Joy1_State						; Copy Player1's inputs for reference 
	sta Input_State                     ; .. during ent logic.
	lda Joy1_Held                       ;
	sta Input_Held                      ;
	lda Joy1_Delta                      ;
	sta Input_Delta                     ;--

	ldx PlayerChars						; Check if Player 1 is active.
	txa									; 
	and #$08							; .. return if they are not active.
	beq @return							;--
	
	txa									; Select player's logic function
	and #$03							;
	asl									;
	tax                 				;
	lda FP_PlayerCharFunctions, x		; .. fetch function pointer ..
	sta v0								;
	lda FP_PlayerCharFunctions + $01, x ;
	sta v1                              ;
	jmp ($0000)							; .. and jump to the target
	
@return:	
	rts
	
; #############################################################################
;  Player 2														   		   [02]
; #############################################################################
Ent_Player2:

	lda Joy2_State						; Copy Player2's inputs for reference 
	sta Input_State                     ; .. during ent logic.
	lda Joy2_Held                       ;
	sta Input_Held                      ;
	lda Joy2_Delta                      ;
	sta Input_Delta                     ;--

	
	lda PlayerChars						; Fetch Player2 flags
	lsr									; .. only shift 3 bits over to save a 
	lsr                                 ; .. little time.
	lsr                                 ;
	tax                                 ;--
	
	and #$40							; Check if Player 2 is active ..
	beq @return							; .. return if they are not active.
	
	txa									; Fetch player character index, since 
	and #$05		                    ; .. we shorted the bitshift earlier, we
	tax                                 ; .. dont need to ASL here.
	lda FP_PlayerCharFunctions, x		; .. Then, fetch function pointer ..
	sta v0								;
	lda FP_PlayerCharFunctions + 1, x   ;
	sta v1                              ;
	jmp ($0000)							; .. and jump to the target

@return:
	rts 
	
; #############################################################################
; Player Character Function Pointers
; #############################################################################
FP_PlayerCharFunctions:
.addr Ent_DKnight
.addr Ent_Test
.addr Ent_Test
.addr Ent_Test

.include "./entities/ent_dknight.asm"	; Diamond Knight [00]