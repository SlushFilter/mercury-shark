; m_initgame.asm
; Init Game - Initializes the game after the players have selected characters.
; Loaded into Bank7
.include "defines.inc"
.include "ram-map.inc"
.autoimport +

.scope NS_INITGAME			; Set NS_INITGAME namespace

; Init Game Local Defines
; ------------------------------------------------------------------------------

SPAWN1_XY			= $02
SPAWN2_XY			= $03

; =============================================================================
; _InitGame
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Runs Stage Initialization Routine.
; =============================================================================
_InitGame:
	jsr F_ClearProtected
	
	lda #$00				; Fetch the pointer to the StageHeader
	sta Level               ;
	lda #$00                ;
	sta Stage               ;
	
	lda #COLOR_BLACK		; Set Background Black to prevent strobing.
	sta BgColor				; --
	
	jsr F_InitPlayers		; Initialize Player 1 and 2
	lda #M_STAGELOAD
	sta ProgramMode
	
	rts

F_InitPlayers:
	; TODO: Player Palettes and Proper stat loading.
	lda PlayerChars
	and #P1_ACTIVE
	beq @initP2
	
	ldx #$00
:	lda D_PlayerCharacter, x
	sta Ent_Data, x
	inx
	cpx #$20
	bne :-

	jsr F_WaitVblank	; Wait for a frame.
	jsr F_DisableGfx
	jsr F_DisableNmi
	
	lda #$00			; Load up Diamond Knight's sprite for now.
	ldy #$01            ; 
	jsr F_LoadTileset   ;--
	
	lda #$01			; Load up Weyrl's sprite for now.
	ldy #$02            ; 
	jsr F_LoadTileset   ;--

	lda #$02			; Load up CSprites
	ldy #$06            ; 
	jsr F_LoadTileset   ;--

@initP2:
	lda PlayerChars
	and #P2_ACTIVE
	beq @return
	
@return:
	jsr F_EnableNmi
	jsr F_EnableGfx
	rts

D_PlayerCharacter:
;	   F   SC  XS   X  YS   Y  VX  	   VY	   ID  MF   W   H
.byte $80,$00,$00,$40,$00,$80,$00,$00,$00,$00,$01,$00,$04,$10,$00,$00
;
.byte $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00


.endscope