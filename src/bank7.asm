; =============================================================================
; Bank 7 - Main Program Bank (Fixed)
; =============================================================================
.include "defines.inc"
.include "ram-map.inc"
.autoimport + 

.define BANK_NUMBER		$07		; Bank Identifier

.segment "BANK7"
.org $C000

; =============================================================================
; Rom Header
; =============================================================================
.byte BANK_NUMBER
.export F_GetBlock, F_SetEntSprite, F_AnimEntSprite, F_ResetEntSprite
.export F_Extend44
; =============================================================================
; Includes
; =============================================================================
.include "ppu.asm"	
.include "m_menu.asm"
.include "m_initgame.asm"
.include "m_stageload.asm"

.include "m_stageintro.asm"
.include "m_stagerun.asm"

; =============================================================================
; Boot Handler
; =============================================================================
DMARKER "### F_ResetHandler ###"
F_ResetHandler:
	sei
	cld
	
	ldx #$FF				; Set stack pointer to #$FF
	txs						;--
	
	jsr F_DisableNmi		; Disable NMI for boot sequence.
	
	clc						; Clear RAM and Seed RNG :3
	ldx #$00				;
	:	adc $00, x          ;
		adc $100, x         ;
		adc $200, x         ;
		adc $300, x         ;
		adc $400, x         ;
		adc $500, x         ;
		adc $600, x         ;
		adc $700, x         ;
		tay                 ;
		lda #$00            ;
		sta $00, x          ;
		sta $100, x         ;
		sta $200, x         ;
		sta $300, x         ;
		sta $400, x         ;
		sta $500, x         ;
		sta $600, x         ;
		sta $700, x         ;
		tya                 ;
		inx                 ;
		bne :-              ;
	sta Rng                 ; --
	

	lda #$00				; Initialize FamiTone5
	jsr F_SelectBank		;
	jsr F_FamitoneInit		; --
	
	jsr F_PpuReset			; Ppu Reset Routine
	
	lda #COLOR_BLACK		; Set global background color.
	sta BgColor				; --
	
	ldx #$20				; Write all black to the palette buffer.
:	sta PaletteBuffer, x	;
	dex                     ;
	bne :-                  ; --
	
	jsr F_WaitVblank		;
	jmp F_Main				;
; =============================================================================
; Main Loop
; =============================================================================
DMARKER "### F_Main ###"
F_Main:								
	jsr F_CallProgramMode			; Transfer control to current program mode.
									; We JSR here because PrograMode uses a tail
									; RTS, and I don't really want to manip the
									; return stack.
	jmp F_Main						; --

F_CallProgramMode:
	lda ProgramMode					; Switch(ProgramMode)
	and #$F0						; Calculate function pointer offset 
	lsr								;
	lsr                             ;
	lsr                             ;
	tax                             ;--
	
	lda FP_ProgramModeFunctions, x		   	; Call Program Mode function ..
	sta v0                          	   	; .. using a tail return to get back
	lda FP_ProgramModeFunctions + $01, x	; .. to F_Main
	sta v1			                        ;
	jmp (v0)        		                ;--

_Boot:								; Boot mode transition is always to the main
	lda #M_MENU						; .. menu.
	sta ProgramMode					;
	rts                             ;--

FP_ProgramModeFunctions:			; Mode Name
.addr _Boot							; [00] M_BOOT
.addr NS_MENU::_MainMenu			; [10] M_MENU
.addr NS_INITGAME::_InitGame		; [20] M_INITGAME
.addr NS_STAGELOAD::_StageLoad		; [30] M_STAGELOAD
.addr NS_STAGEINTRO::_StageIntro	; [40] M_STAGEINTRO
.addr NS_STAGERUN::_StageRun		; [50] M_STAGERUN
.addr _Boot							; [60] *unused*
.addr _Boot							; [70] *unused*
.addr _Boot							; [80] *unused*
.addr _Boot							; [90] *unused*
.addr _Boot							; [A0] *unused*
.addr _Boot							; [B0] *unused*
.addr _Boot							; [C0] *unused*
.addr _Boot							; [D0] *unused*
.addr _Boot							; [E0] *unused*
.addr _Boot							; [F0] *unused*

; =============================================================================
; Nmi Handler
; =============================================================================
DMARKER "### F_NmiHandler ###"
F_NmiHandler:
	php						; Backup Processor / Register State
	pha						;
	txa     				;
	pha     				;
	tya     				;
	pha     				; 
	lda PrgBank				; .. and the Currently load PRG bank.
	sta LastBank			; 

	jsr F_PpuService		; Process the PpuService buffer.

	bit PpuOverride			; Check to see if PpuOverride is flagged ..
	bmi @skipPpuUpdates		; .. Skip updates that alter PPUADDR if so.

	
	lda #$3F				; Update background Color
	sta PPUADDR             ;
	lda #$00                ;
	sta PPUADDR             ;
	lda BgColor             ;
	sta PPUDATA             ;--

	lda PpuMaskShadow       ; Update PPUMASK
	sta PPUMASK             ;

	jsr F_UpdateScroll		; Update Scroll Registers

	lda PpuCtrlShadow		; Updaet PPUCTRL
	sta PPUCTRL             ;
	
@skipPpuUpdates:


    lda #$02        		; Write OAM Data.
    sta OAMDMA      		;
	lda #$00				; Reset OAM Index
	sta OamIndex			;--

	jsr F_PollInput			; Update Controller Input
	
	lda #BANK_AUDIO			; Audio update tick
	jsr F_SelectBank        ; 
	jsr F_FamitoneUpdate	;--
	
	inc Rng					; Rng++ :)

	lda LastBank			; Restore the last PRG bank.
	jsr F_SelectBank		; --

	pla						; .. and Processer / Register State
	tay						;
	pla						;
	tax						;
	pla						;
	plp						;
	rti						; --

; =============================================================================
; Brk Handler
; =============================================================================
F_BrkHandler:
	rti

; =============================================================================
; F_PollInput
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Reads int the joypad presses and stores buttons for the frame as well
; as buttons that changed since last frame.
; =============================================================================
F_PollInput:
	lda #$00
	sta n0					; Joypad 1 Temp Buttons for this frame.
	sta n1					; Joypad 2 Temp Buttons for this frame.
	
	ldx #$00
	
@readJoys:	
	lda #$01				; Strobe the Joy1 port to prep for a read.
	sta JOY1                ; 
	lda #$00                ; 
	sta JOY1                ; --

	ldy #$08				; Read Joypad Pass 1
:	lda JOY1, x				; .. store them in n0 (Joy1) or n1 (Joy2) 
	and #$01				;
	lsr						;
	lda n0					;
	rol						;
	sta n0					;
	dey						;
	bne :-					;

	lda #$01				; Strobe the Joy1 port to prep for a read.
	sta JOY1                ; 
	lda #$00                ; 
	sta JOY1                ; --

	ldy #$08				; Read Joypad Pass 2
:	lda JOY1, x				; .. store them in n0 (Joy1) or n1 (Joy2) 
	and #$01				;
	lsr						;
	lda n1					;
	rol						;
	sta n1					;
	dey						;
	bne :-					;
	lda n0
	cmp n1
	beq @commitInput
							; Inputs didnt match, read again and pick the 
							; matching input.
	lda #$01				; Strobe the Joy1 port to prep for a read.
	sta JOY1                ; 
	lda #$00                ; 
	sta JOY1                ; --

	ldy #$08				; Read Joypad Pass 3
:	lda JOY1, x				; .. store them in n0 (Joy1) or n1 (Joy2) 
	and #$01				;
	lsr						;
	lda n2					;
	rol						;
	sta n2					;
	dey						;
	bne :-					;
	
	lda n2					; Check if n2 read == n1 read
	cmp n1					;
	beq @commitInput		; .. if they match it was a good read.
	lda n0					; Otherwise n0 must have been the good read.
@commitInput:
	tay						;
	and Joy1_State, x	    ;
	sta Joy1_Held, x        ;
	tya                     ;
	eor Joy1_State, x       ;
	sta Joy1_Delta, x       ;
	sty Joy1_State, x       ;--

	inx						; Loop for both controllers.
	cpx #$02				;
	bne @readJoys			;--
	
	rts

; =============================================================================
; F_QueueSfx
;
; Queues a Sound Effect in one of the 4 sound effect channels to be played by
; Famitone.
;
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Arguments :
; A : Sound Effect Number
; X : Sound Effect Priority
; =============================================================================
F_QueueSfx:
	tay
@nextChan:
	lda SFX_0, x	; Check for open SFX channel
	cmp #$FF		;
	beq @openChan	;--
	
	inx				; Get the next channel
	cmp #$04		;
	bcc @nextChan   ;--
	rts				; No open channel, return.

@openChan:			
	tya				; Write SFX index to open channel and return.
	sta SFX_0, x    ;
	rts             ;--

; =============================================================================
; F_SelectBank
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Selects a PRG bank in LoRom $8000
; 
; Arguments
; A : The bank to select
;
; Volitile
; X : Used as a temporary register in this routine.
; =============================================================================
F_SelectBank:
	and #$07		
	tax
:	lda D_BankTable, x		; Perform the bank switch 
	sta D_BankTable, x		; 
	cmp BANK_ID				; Make sure we have a successful bank switch.
	bne :-					; .. hammer it if we dont.
	sta PrgBank
	rts

D_BankTable:
.byte $00, $01, $02, $03, $04, $05, $06

; =============================================================================
; F_ClearProtected
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Clears process "protected" Ram (m0:mF)
; =============================================================================
F_ClearProtected:
	ldy #$0F
	lda #$00
:	sta m0, y
	dey
	bne :-
	rts

; =============================================================================
; F_StartTimer
;
; Waits for X seconds and Y Frames, or until the player presses start.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Arguments:
; X : Number of seconds to wait.
; Y : Number of frames to wait.
; =============================================================================
F_StartTimer:
			jsr F_WaitVblank
			dey
			bne @checkStart
			dex
			beq @timerDone
			ldy #$3C
@checkStart:
			lda Joy1_Delta
			and Joy1_State
			and #JOY_START
			beq F_StartTimer
@timerDone:	
			rts

; =============================================================================
; F_WaitTimer
;
; Waits for X seconds and Y Frames
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Arguments:
; X : Number of seconds to wait.
; Y : Number of frames to wait.
; =============================================================================
F_WaitTimer:
			jsr F_WaitVblank
			dey
			bne F_WaitTimer
			dex
			beq @timerDone
			ldy #$3C
			bne F_WaitTimer
@timerDone:	
			rts

; =============================================================================
; F_Add16
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 16 Bit addition routine
; * Shreds v0 and v1
;
; Arguments :
;  A : Lower 8 bits of (word)A
;  X : Upper 8 bits of (word)A
; v0 : Lower 8 bits of (word)B
; v1 : Upper 8 bits of (word)B
;
; Returns :
; A : Lower 8 bits of addition.
; X : Upper 8 bits of addition.
; =============================================================================
F_Add16:
	clc		;
	adc v0  ; Add lower byte ..
	sta v0  ;
	bcc :+  ;
	inx  	; .. increment x if there was a carry.
	clc		; 
:	txa     ; .. add upper byte ..
	adc v1  ;
	sta v1  ;
	tax     ;
	lda v0  ; 
	rts     ; return (word)A:X

; =============================================================================
; F_Extend44
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Sign extend a 4.4 fixed point encoded variable and store the results in X
; and Y.
;
; Arguments : 
; A - 4.4 fixed point encoded variable
; Returns :
; Y - Sign extended whole portion
; A - Sign extended fractional portion
; =============================================================================
F_Extend44:
	cmp #$80			; Check for a sign
	bcs @signExtend
	
	pha					; Push A for later retreival 
	lsr                 ;
	lsr                 ;
	lsr                 ;
	lsr                 ;
	and #$0F            ; .. set Y to the upper nybble of the argument.
	tay                 ; --
	
	pla					; Restore the fractional portion of the fixed point var.
	asl					;
	asl                 ;
	asl                 ;
	asl                 ;
	and #$F0            ; --
	rts                 
	
@signExtend:
	pha					; Push A to sign extend the low nybble later.
	lsr					;
	lsr                 ;
	lsr                 ;
	lsr                 ;
	ora #$F0            ; .. sign extend the byte and transfer to Y
	tay                 ; --
	
	pla					; Sign extend the low nybble, 2's compliment
	asl					;
	asl                 ;
	asl                 ;
	asl                 ;
	clc					;
	adc #$01            ;
	eor #$FF            ;--
	
@return:
	rts
; =============================================================================
; F_GetBlock
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Fetches a block from the current requested screen blockmap.
; * Shreds v0 and v1
;
; X : Entity X Coordinate to test.
; Y : Entity Y Coordinate to test.
; A : Screen to test.
;
; Returns :
; A : The blockmap data at that location.
; =============================================================================
F_GetBlock:
	sta v2		; v2 = Screen Index

	txa			; Calculate Offset to Queried blockmap.
	lsr			; Offset = (X / 32) + ((Y / 32) * 8)
	lsr         ;
	lsr         ;
	lsr         ;
	lsr         ;
	sta v0		; .. v0 = X >> 5
	tya			;
	and #$E0    ; 
	lsr         ; 
	lsr         ;
	ora v0      ; .. v0 += (Y & #$E0) >> 2 
	sta	v0		; --

	lda PrgBank				; Switch to stage data bank for the fetch.
	sta v1              	;
	lda #BANK_STAGEDATA 	;
	jsr F_SelectBank    	; --
	
	ldy v2					; Y = Screen Index
	lda (BaseStageMap), y	; --
	and #$0F
	
	clc
	tay
	lda D_Mult64_LO, y
	adc BaseBlockMap
	sta v2
	lda D_Mult64_HI, y
	adc BaseBlockMap + 1
	sta v3
	
	ldy v0 					; Fetch the tile
	lda (v2), y				; -- 
	
	tay				 		; Restore previous LOROM bank.
	lda v1		 			;
	jsr F_SelectBank 		;
	tya              		;--
	
	rts
	
D_Mult64_LO:
	.byte $00, $40, $80, $C0, $00, $40, $80, $C0
	.byte $00, $40, $80, $C0, $00, $40, $80, $C0
D_Mult64_HI:
	.byte $00, $00, $00, $00, $01, $01, $01, $01
	.byte $02, $02, $02, $02, $03, $03, $03, $03

; =============================================================================
; F_SetEntSprite
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Set the current entity animation index and related pointers.
; Routine will exit if the animation is already set to the requested animation
; index.
;
; Arguments 
; Y : Requested animation index.
; =============================================================================
F_SetEntSprite:
	cpy Ent_AnimIndex
	bne F_ResetEntSprite
	rts
	
; =============================================================================
; F_ResetEntSprite
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Resets the entity sprite and reloads initial values for FramePtr, FrameIndex
; and FrameTimer
; =============================================================================
F_ResetEntSprite:
	sty Ent_AnimIndex
	
	lda PrgBank
	pha

	lda #BANK_SPRITES	; Load up the sprite bank.
	jsr F_SelectBank    ;--
	
	; Get Offset to AnimPointer
	lda #$00
	sta v1
	clc
	tya
	asl
	rol v1
	asl
	rol v1
	sta v0
	lda #<D_AnimPtrs
	adc v0
	sta v0
	lda #>D_AnimPtrs
	adc v1
	sta v1
	
	ldy #$00
	lda (v0), y
	iny
	sta Ent_FramePtr_LO
	
	lda (v0), y
	iny
	sta Ent_FramePtr_HI
	
	lda (v0), y
	iny
	sta Ent_FrameCount
	
	lda (v0), y
	sta Ent_FrameTimer
	
	pla					; Restore Program Bank
	jsr F_SelectBank	; --
	
@return:
	rts


; =============================================================================
; F_AnimEntSprite
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; =============================================================================
F_AnimEntSprite:
	lda Ent_FrameTimer	; Check for frame timer expiration.
	and #$0F			;
	beq @advanceFrame	; .. update the Frame index if expired.
	dec Ent_FrameTimer	; .. otherwise decrement the frame timer and exit.
	rts					;--
	
@advanceFrame:
	ldy Ent_AnimIndex
	dec Ent_FrameCount		; Decrement the frame index
	beq F_ResetEntSprite	; .. Reset the animation if the anim is looping.
	
	lda PrgBank				; Store current prg bank.
	pha						; Load up the sprite bank.
	lda #BANK_SPRITES		; 
	jsr F_SelectBank    	;--

	ldy #$00				; Update Ent_FramePtr to next frame.
	lda (Ent_FramePtr), y	; 
	clc                     ; .. Ent_FramePtr += (SpriteCount * 4) + 1
	asl                     ;
	asl                     ;
	adc #$01                ;
	adc Ent_FramePtr_LO		;
	sta Ent_FramePtr_LO		;
	bcc @resetTimer			;
	inc Ent_FramePtr_HI		;--
	
@resetTimer:
	lda Ent_FrameTimer		; Reset Ent_FrameTimer
	lsr						;
	lsr                     ;
	lsr                     ;
	lsr                     ;
	ora Ent_FrameTimer      ;
	sta Ent_FrameTimer      ;--

	pla						; Restore bank.
	jsr F_SelectBank		;--
	
@return:	
	rts 

; =============================================================================
; Interrupt Vectors
; =============================================================================
.segment "IVECT"
.word F_NmiHandler
.word F_ResetHandler
.word F_BrkHandler