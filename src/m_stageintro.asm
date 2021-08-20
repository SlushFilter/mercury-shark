; m_stageload.asm
; Stage Load - Routine to load up a stage, assumes players have just entered the
; 			   stage.
; Loaded into Bank7
.include "defines.inc"
.include "ram-map.inc"
.autoimport +

DMARKER "### _StageIntro ###"
.scope NS_STAGEINTRO		; Set NS_STAGEINTRO namespace

; Stage Intro Local Defines
; -----------------------------------------------------------------------------


; Vram Address Pointers
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
VRAM_LEVELTEXT		:= $2508	; Vram address for Level X Stage Y position.
VRAM_LEVELTEXT_X	:= VRAM_LEVELTEXT + $06
VRAM_LEVELTEXT_Y	:= VRAM_LEVELTEXT + $10

VRAM_STAGETEXT		:= $2570	; Vram address for stage name centerprint.

; Scrolling speed
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
SCROLL_SPEED 		= $F8

_StageIntro:
	jsr F_ClearProtected
	jsr F_WaitVblank			; Waitf for a frame.
	
	lda #BANK_STAGEDATA			; Select stage data bank.
	jsr F_SelectBank			;--

	jsr F_DisableGfx			; Draw the Level Text on Nametable 2
	ldy #$01					;
	jsr F_ClearNameTable		;
	jsr F_DrawLevelText			;
	jsr F_EnableGfx				;--
	
	lda #$08
	jsr F_FadeIn
	
	ldx #$03					; Wait for 3 seconds or start press.
	ldy #$3C					;
	jsr F_StartTimer			;--
	
	
:	lda #SCROLL_SPEED			; Scroll the initial playfield into view.
	jsr F_Scroll				;
	jsr F_WaitVblank            ;
	lda Scroll_XHI              ;
	beq :-                      ;
	lda #$00                    ;
	sta Scroll_X                ;
	sta Scroll_XHI              ;--
	lda #$1F
	sta SeamColumn
	lda #M_STAGERUN
	sta ProgramMode
	rts
; =============================================================================
; F_DrawLevelText																	
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Draws "Level X - Stage Y" and the sage name on Nametable 2
; =============================================================================
F_DrawLevelText:
	ldx PpuBufferWrite			; Set text write address.
	lda #<VRAM_LEVELTEXT        ;
	ldy #>VRAM_LEVELTEXT       	;
	jsr F_PpuBufferPushWord     ;--
	lda D_LevelText          	; Write the data length to PpuBuffer
	sta PpuBuffer, x            ;
	inx                         ;--
	lda #<D_LevelText			; Write "LEVEL X - STAGE Y" to PpuBuffer
	sta v0						;
	lda #>D_LevelText           ;
	sta v1                      ;
	jsr F_PpuBufferPushText     ;--
	
	
	lda #<VRAM_LEVELTEXT_X		; Write Level #
	ldy #>VRAM_LEVELTEXT_X      ;
	jsr F_PpuBufferPushWord     ;
	clc                         ;
	lda Level                   ;
	adc #$10                    ;
	tay                         ;
	lda #$01                    ;
	jsr F_PpuBufferPushWord     ;--

	lda #<VRAM_LEVELTEXT_Y		; Write Level #
	ldy #>VRAM_LEVELTEXT_Y      ;
	jsr F_PpuBufferPushWord     ;
	clc                         ;
	lda Stage                   ;
	adc #$10                    ;
	tay                         ;
	lda #$01                    ;
	jsr F_PpuBufferPushWord     ;--
	
	jsr NS_STAGELOAD::F_GetStageIndex
	
	lda DP_StageNames, y		; Write Source string data pointer to v0
	sta v0                      ;
	lda DP_StageNames + 1, y    ;
	sta v1                      ;--
	
	
	ldy #$00					; Find centerprint offset.
	lda (v0), y					;
	lsr							; .. divide by 2
	clc							;
	eor #$FF					; .. negate
	adc #$01					;--
	
	adc #<VRAM_STAGETEXT		; Calculate centerprint offset.
	ldy #>VRAM_STAGETEXT		;--
	jsr F_PpuBufferPushWord
	ldy #$00
	lda (v0), y					; Write Data Length
	sta PpuBuffer, x			;
	inx							;--
	jsr F_PpuBufferPushText		; Draw the string.
	
	stx PpuBufferWrite          ;
	
	rts							;--

D_LevelText:
PSTRING "LEVEL X - STAGE Y"

.endscope