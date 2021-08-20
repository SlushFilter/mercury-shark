; ppu.a
; Basic PPU routines
; Loaded into Bank7
.include "defines.inc"
.include "ram-map.inc"
.autoimport +

; =============================================================================
; F_WaitVblank
; =============================================================================
F_WaitVblank:
	lda Rng
:	cmp Rng
	beq :-
	rts
	
; =============================================================================
; F_PpuReset
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Boot code for the PPU, it zeros out PPU memory, used during system reboot.
; =============================================================================

F_PpuReset: 
	lda #%10010000		; Disable Display and set defaults.
	sta PpuCtrlShadow	;
	sta PPUCTRL			;
	lda #%00000110		;
	sta PpuMaskShadow	;
	sta PPUMASK			; --
	
	jsr F_EnableNmi		; Wait for PPU Warmup (6 frames)
	:   bit PPUSTATUS	;
	    bpl :-          ;
	:   bit PPUSTATUS   ;
	    bpl :-          ;
	:   bit PPUSTATUS   ;
	    bpl :-          ;
	:   bit PPUSTATUS   ;
	    bpl :-          ;
	:   bit PPUSTATUS   ;
	    bpl :-          ;
	:   bit PPUSTATUS   ;
	    bpl :-          ;
	jsr F_DisableNmi    ; --
	
	jsr F_HideSprites	; Clear OAM table.
	
	lda #$00        	; OAMADDR Config - Set to offset #$00
    sta OAMADDR     	; --
	
	jsr F_ResetScroll   ; Reset Scroll Registers
	
    lda #$02        	; Clear PPU OAM data
    sta OAMDMA      	;

    jsr F_ClearVram   	; Clear VRam
	jsr F_EnableGfx		; Turn sprites / background on.
	
; . . Fall into F_EnableNmi

; =============================================================================
; F_EnableNmi
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Enables NMI generation on the PPU
; =============================================================================
F_EnableNmi:
	lda PpuCtrlShadow
	ora #%10000000
	sta PpuCtrlShadow
	sta PPUCTRL
	rts
     
; =============================================================================
; F_DisableNmi
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Disables NMI generation on the PPU
; =============================================================================
F_DisableNmi:
	lda PpuCtrlShadow
	and #%01111111
	sta PpuCtrlShadow
	sta PPUCTRL
	rts

; =============================================================================
; F_EnableGfx
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Enables background and sprite rendering.
; =============================================================================
	
F_EnableGfx:
	lda PpuMaskShadow
	ora #%00011000
	sta PpuMaskShadow
	sta PPUMASK
	rts

; =============================================================================
; F_DisableGfx
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Disables background and sprite rendering.
; =============================================================================
F_DisableGfx:
	lda PpuMaskShadow
	and #%11100111
	sta PpuMaskShadow
	sta PPUMASK
	rts

; =============================================================================
; F_ClearVram
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Clears VRam
; =============================================================================
F_ClearVram:
	lda #$3F
	sta PPUADDR
	lda #$00
	sta PPUADDR
	lda #$0D
	ldy #$20
:	sta PPUDATA
	dey
	bne :-
	
    lda #$00        
    ldx #$40
    ldy #$00
    sta PPUADDR     ; Set PPU Address to $0000
    sta PPUADDR     ;
    
:   sta PPUDATA     ; Writes 3FFF bytes to the PPU, clearing it.
    dey             ;
    bne :-          ;
    dex             ;
    bne :-          ; --
    rts

; =============================================================================
; F_DisablePpuService
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Disables the majority of PPU Service routines, namely the ones that mess with
; VRAM Addresses
; =============================================================================
F_DisablePpuService:
	lda PpuOverride		; Disable PPU Service routines that update addresses.
	ora #%10000000		;
	sta PpuOverride		;--
	rts

; =============================================================================
; F_EnablePpuService
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Returns the PpuService back to normal operating mode.
; =============================================================================
F_EnablePpuService:
	lda PpuOverride		; Disable PPU Service routines that update addresses.
	and #%000000000		;
	sta PpuOverride		;--
	rts

; =============================================================================
; F_ClearNameTable
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Clears a nametable to #$00
;
; Y : NameTable to clear (0) $2000, (1)$2400, (2)$2800, (3)$2C00
; =============================================================================

F_ClearNameTable:
	jsr F_DisablePpuService
	
	lda PpuMaskShadow	; Disable background / sprite rendering.
	and #%11100111      ;
	sta PPUMASK         ;--
	
	lda PpuCtrlShadow	; Set PPU write mode for "across" 
	and #%11111011		;
	sta PPUCTRL			;--
	
	tya					; Select name table base address.
	and #$03            ;
	tay                 ;
	lda D_NTAddr_HI, y	;--
	
	sta PPUADDR			; Set PpuAddress to nametable.
	lda #$00            ;
	sta PPUADDR         ;--
	
	ldy #$00 			; Write #$400 bytes to PPUDATA
@loop:                  ;
	sta PPUDATA         ;
	sta PPUDATA         ;
	sta PPUDATA         ;
	sta PPUDATA         ;
	dey                 ;
	bne @loop           ;--

	jsr F_UpdateScroll
	jsr F_EnablePpuService
	rts

D_NTAddr_HI:
.byte $20, $24, $28, $2C

; =============================================================================
; F_HideSprites
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Clears Oam RAM by hiding all sprites offscreen.
; =============================================================================
F_HideSprites:
	ldy #$00
	lda #$FF
:	sta OamRam, y
	iny
	iny
	iny
	iny
	bne :-
	rts
	
; =============================================================================
; F_ResetScroll
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Reset Scroll Registers to #$00
; =============================================================================
F_ResetScroll:
	lda #$00
	sta Scroll_X		;
	sta Scroll_XHI    	;
	sta Scroll_Y		; Reset Scrolling
    sta PPUSCROLL   	;
    sta PPUSCROLL   	; --
	rts

; =============================================================================
; F_UpdateScroll
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Update Scroll Registers to Scroll_X and Scroll_Y
; =============================================================================
F_UpdateScroll:			
	lda Scroll_X        ; Set the PPUSCROLL registers to shadow values.
	sta PPUSCROLL       ;
	lda Scroll_Y        ;
	sta PPUSCROLL       ; --
	
	lda PpuCtrlShadow	; Set PpuCtrlShadow base nametable.
	and #%11111100	    ; .. Default base nametable = $2000
	ldx Scroll_XHI		; .. Check Scroll_XHI
	beq :+				;
	ora #%00000001		; .. Set base nametable = $2400 if Scroll_XHI != #$00
:	sta PpuCtrlShadow	;--
	rts

; =============================================================================
; F_Scroll
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Scroll by X pixels
; 
; A : Signed value to scroll by.
; =============================================================================
F_Scroll:
	cmp #$80			; Check for negative or positive value.
	bcs @scrollRight	;--
	
	clc
	adc Scroll_X		; Scroll Left 
	sta Scroll_X        ; .. add A to the Scroll_X register.
	lda #$00            ;
	adc Scroll_XHI      ;
	and #$01            ;
	sta Scroll_XHI      ;
	rts                 ;--

@scrollRight:
	ldy Scroll_X
	
	clc
	adc Scroll_X
	sta Scroll_X
	cpy Scroll_X
	bcs :+
	lda Scroll_XHI
	eor #$01
	sta Scroll_XHI 
:	rts
	
; =============================================================================
; F_LoadTileset:
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Loads a tileset from ROM into VRam at the given tile page.
; * Background and sprites should be disabled for this operation.
;
; A : Target TileSet Address (Slot 0-7)
; Y : Source Tileset Index
; =============================================================================
F_LoadTileset:
		sta v0				; v0 = Target Tileset Address
		sty v1				; v1 = Source Tileset Index
		
		jsr F_DisablePpuService
	
		lda PpuMaskShadow	; Disable background / sprite rendering.
		and #%11100111      ;
		sta PPUMASK         ;--
	
		lda PpuCtrlShadow	; Set PPU write mode for "across" 
		and #%11111011		;
		sta PPUCTRL			;--
		
		lda v0
		ldy v1
		
		clc					; Calculate target VRam Address
		asl					;
		asl                 ;
		sta PPUADDR         ;
		lda #$00            ;
		sta PPUADDR         ;--
		
		lda v0				; Select Bank 1 (Sprite CHR) or Bank 2 (Tile CHR)
		and #$0C			; Depending on target Pattern table index.
		beq :+              ;
		lda #$01            ;
	:	adc #$01            ;
		jsr F_SelectBank	;--
		
		lda #<DP_TileSets	; Fetch source data pointer to tile data.
		sta v0              ;
		lda #>DP_TileSets   ;
		sta v1              ;
		clc					;
		tya					;
		asl					;
		bcc :+				;
		inc v1              ;
	:	adc v0              ;
		sta v0              ;
		bcc :+              ;
		inc v1              ;--
		
	:	ldy #$00			; Dereference source data pointer.
		lda (v0), y			;
		sta v2				;
		iny					;
		lda (v0), y			;
		sta v3				;--
		
		ldx #$04			; $400 bytes total to copy, so we copy $100 bytes
@outer:	ldy #$00			;  4 times.
		clc                 ;
@inner:	lda (v2), y      	;
		sta PPUDATA         ;
		iny                 ;
		bne @inner          ;
		inc v3				; inc source data pointer every pass.
		dex					;
		bne @outer			;--
		
		jsr F_EnablePpuService

		rts

; =============================================================================
; F_LoadNametable
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Loads a full NameTable from ROM
;
; A : Target Nametable (0 or 1)
; Y : Source nametable index.
; =============================================================================
F_LoadNametable:
		and #$01			; v0 = Target Nametable
		sta v0				; --
		sty v1				; v1 = Source Nametable Idx
		
		jsr F_DisablePpuService

	
		lda PpuMaskShadow	; Disable background / sprite rendering.
		and #%11100111      ;
		sta PPUMASK         ;--
	
		lda PpuCtrlShadow	; Set PPU write mode for "across" 
		and #%11111011		;
		sta PPUCTRL			;--
		
		lda #BANK_NTPAL		; Select Bank 6, this is where nametables live.
		jsr F_SelectBank	;--

		ldy v0				; Set PPUADDR
		lda #$20			;
		cpy #$00			;
		beq :+              ;
		lda #$24            ;
	:	sta PPUADDR         ;
		lda #$00            ;
		sta PPUADDR         ;--
		
		clc					; Get nametable data pointer.
		lda v1              ;
		asl                 ;
		adc #<DP_Nametables ;
		sta v2              ;
		lda #>DP_Nametables ;
		adc #$00            ;
		sta v3              ;--
		
		ldy #$00			; Dereference Data Pointer
		lda (v2), y         ;
		iny                 ;
		sta v0              ;
		lda (v2), y      	;
		sta v1              ;--

@fetchControlByte:			
		ldy #$00
		lda (v0), y			; Fetch a control byte.
		beq @return			; [00] Exit routine
		bmi @rleRead		; [80] RLE Byte
		
@rawRead:					; Perform RAW Data read / write
		iny                 ; 
		tax		            ; 
	:	lda (v0), y         ; 
		sta PPUDATA         ; 
		iny                 ; 
		dex                 ; 
		bne :-              ; 
		beq @nextControl    ;-- 
		
@rleRead:
		iny					; Perform RLE Data read / write
		and #$7F			;
		tax                 ;
		lda(v0), y          ;
		iny                 ;
    :	sta PPUDATA         ;
		dex                 ;
		bne :-              ;--
		
@nextControl:
		clc						; Update Source Data Pointer
		tya                     ;
		adc v0                  ;
		sta v0                  ;
		bcc :+                  ;
		inc v1                  ;
:		jmp @fetchControlByte   ;--
		
@return:
		jsr F_UpdateScroll
		jsr F_EnablePpuService
		rts

; =============================================================================
; F_LoadPalette
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Loads a BG or FG palette into the PaletteBuffer. Call F_CommitPalette to write
; it to the PPU
; * Uses v0, v1, v2, and v3
;
; A : Target Palette (0 - 8)
; Y : Index of D_Palettes to load
; =============================================================================
F_LoadPalette:
	asl					; Save target index for later
	asl         		;
	sta v2      		; --
	
	clc					; Calc the source data offset.
	lda #$00			; 
	sta v1				; .. (word)v0 = Y * 3
	tya                 ; .. since palette colors are packed in sets of 3
	sta v0              ;
	asl                 ;
	rol v1              ;
	adc v0              ;
	sta v0              ;
	lda #$00            ;
	adc v1              ;
	sta v1				; 
	lda #<D_Palettes	; .. Finish up with the 16 bit add of D_Palettes
	adc v0              ;
	sta v0              ;
	lda #>D_Palettes    ;
	adc v1              ;
	sta v1              ;--
	
	

	lda PrgBank			; Save the last PrgBank before switching.
	sta v3				;
	lda #BANK_NTPAL		; Switch to GFX bank.
	jsr F_SelectBank	; --
	ldx v2				; Target data indexer.
	ldy #$00			; All setup, lets load some data.
	
	lda BgColor				; Write data to the PaletteBuffer
	sta PaletteBuffer, x    ;
	lda (v0), y             ;
	iny                     ;
	inx                     ;
	sta PaletteBuffer, x    ;
	lda (v0), y             ;
	iny                     ;
	inx                     ;
	sta PaletteBuffer, x    ;
	lda (v0), y             ;
	iny                     ;
	inx                     ;
	sta PaletteBuffer, x    ; --
	inx
	lda v3				; Restore PrgBank
	jsr F_SelectBank	; --
	
	rts

; =============================================================================
; F_CommitPalette
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Pushes the current palette onto the PpuBuffer buffer to be written to Vram
; =============================================================================
F_CommitPalette:
	lda #<PaletteBuffer
	sta v0
	lda #>PaletteBuffer
	sta v1
	ldx PpuBufferWrite
	ldy #$3F
	lda #$00
	jsr F_PpuBufferPushWord
	lda #$20
	sta v2
	jsr F_PpuBufferPushByte
	jsr F_PpuBufferPush
	stx PpuBufferWrite
	rts
	
; =============================================================================
; F_Blackout
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Blacks out the screen with a palette load, does not effect PaletteBuffer
; =============================================================================
F_Blackout:
	lda #COLOR_BLACK
	sta BgColor
	lda #<D_BlackoutString
	sta v0
	lda #>D_BlackoutString
	sta v1
	lda #$04
	sta v2
	ldx PpuBufferWrite
	jsr F_PpuBufferPush
	stx PpuBufferWrite
	rts
	
D_BlackoutString:
.byte $00, $3F, $A0, $0F

; =============================================================================
; F_FadeOut
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 4 step fade routine, does not change the PaletteBuffer, but fades from the
; current PaletteBuffer settings to black.
;
; * Modifies v0, v1
;
; Arguments :
; A : Number of frames to wait between fade steps (minimum 1)
;
; =============================================================================

F_FadeOut:
	cmp #$01					; Floor frame delay to 1 and store it in v1
	bcs :+						;
	lda #$01                    ;
:	sta v1						; --

	lda #$00					; v0 = Fade difference accumulator
	sta v0						; --

@fadeLoop:
	ldx PpuBufferWrite			; Write VRAM target and data length to PpuBuffer
	ldy #$3F                    ;
	lda #$00                    ;
	jsr F_PpuBufferPushWord     ;
	lda #$20                    ;
	jsr F_PpuBufferPushByte     ; --
	
	clc							; Update fade difference
	lda #$10					;
	adc v0                      ;
	sta v0                      ; --
	
	ldy #$00
	
@writeLoop:						; Adjust the palette color and write 32 bytes to
	sec                         ; .. PppuBuffer
	lda PaletteBuffer, y        ;
	sbc v0                      ;
	bcs :+                      ;
	lda #COLOR_BLACK            ;
:	sta PpuBuffer, x            ;
	inx                         ;
	iny                         ;
	cpy #$20                    ;
	bne @writeLoop              ; --
	
	stx PpuBufferWrite			; Commit the write.
	
	ldx v1						; Wait for X frames
:	jsr F_WaitVblank			;
	dex							;
	bne :-                      ; --
	
	lda v0						; Check to see if we have any more steps.
	cmp #$40                    ;
	bne @fadeLoop               ; --
	
	rts							; All done return!

; =============================================================================
; F_FadeIn
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; 4 step fade routine, does not change the PaletteBuffer, but fades from black
; to the current PaletteBuffer colors.
;
; * Modifies v0, v1
;
; Arguments :
; A : Number of frames to wait between fade steps (minimum 1)
;
; =============================================================================

F_FadeIn:
	cmp #$01					; Floor frame delay to 1 and store it in v1
	bcs :+						;
	lda #$01                    ;
:	sta v1						; --

	lda #$40					; v0 = Fade difference accumulator
	sta v0						; --

@fadeLoop:
	ldx PpuBufferWrite			; Write VRAM target and data length to PpuBuffer
	ldy #$3F                    ;
	lda #$00                    ;
	jsr F_PpuBufferPushWord     ;
	lda #$20                    ;
	jsr F_PpuBufferPushByte     ; --
	
	sec							; Update fade difference
	lda v0						;
	sbc #$10                    ;
	sta v0                      ; --
	
	ldy #$00
	
@writeLoop:						; Adjust the palette color and write 32 bytes to
	sec                         ; .. PppuBuffer
	lda PaletteBuffer, y        ;
	sbc v0                      ;
	bcs :+                      ;
	lda #COLOR_BLACK            ;
:	sta PpuBuffer, x            ;
	inx                         ;
	iny                         ;
	cpy #$20                    ;
	bne @writeLoop              ; --
	
	stx PpuBufferWrite			; Commit the write.
	
	ldx v1						; Wait for X frames
:	jsr F_WaitVblank			;
	dex							;
	bne :-                      ; --
	
	lda v0						; Check to see if we have any more steps.
	bne @fadeLoop               ; --
	
	rts							; All done return!


; =============================================================================
; F_PpuBufferPushText
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Writes a Pascal String to PPU Buffer.
;
; Arguments :
; v0:v1 (word) : Source Pstring Data Pointer
;
; Returns :
; X : PpuBufferWrite
; =============================================================================

F_PpuBufferPushText:
	clc
	ldy #$00			; Initialize indexer and read in data length.
	lda (v0), y			; 
	adc #$01
	sta v2				;--
	iny
:	lda (v0), y			; Copy source data to buffer.
	sec					;
	sbc #$20			; .. convert ascii character
	sta PpuBuffer, x	;
	inx					;
	iny                 ;
	cpy v2              ;
	bne :-              ;--
	
	rts

; =============================================================================
; F_PpuBufferPush
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Writes A bytes to the PpuBuffer from the source address in X, Y
;
; Arguments :
; v0:v1 (word) : Source Data Pointer
; v2		   : Data Length
;
; Returns :
; X : PpuBufferWrite
; =============================================================================
F_PpuBufferPush:
	ldy #$00			; Initialize indexer.
						; --
	
:	lda (v0), y			; Copy source data to buffer.
	sta PpuBuffer, x	;
	inx					;
	iny                 ;
	cpy v2              ;
	bne :-              ;--
	
	rts

; =============================================================================
; F_PpuBufferPushWord
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Writes XY to the PpuBuffer, but does not update PpuBufferWrite.
; To commit the write, update PpuBufferWrite with the return value.
;
; Arguments :
;	X : PpuBufferWrite
; 	A : First Byte to push.
; 	Y : Second Byte to push.
; Returns :
;   X : PpuBufferWrite
; =============================================================================
F_PpuBufferPushWord:
	sta PpuBuffer, x	; Very simple, just push two bytes and advance the ..
	inx                 ; .. indexer.
	tya                 ;
	sta PpuBuffer, x    ;
	inx                 ;--
	rts

; =============================================================================
; F_PpuBufferPushByte
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Writes XY to the PpuBuffer, but does not update PpuBufferWrite.
; To commit the write, update PpuBufferWrite with the return value.
;
; Arguments :
;	X : PpuBufferWrite
; 	A : Byte to push.
; Returns :
;   X : PpuBufferWrite
; =============================================================================
F_PpuBufferPushByte:
	sta PpuBuffer, x	; Very simple, just push a byte and advance the ..
	inx                 ; .. indexer.
	rts

; =============================================================================
; F_PpuService
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Writes command data from the Ppu Data Buffer to the PPU
; =============================================================================
F_PpuService:
	ldx PpuBufferRead		; Check to see if there's any pending data in the
@checkBuffer:				;
	cpx PpuBufferWrite      ; buffer.
	bne :+             		;--
	
	@return:				; Done writing, exit the routine.
	stx PpuBufferRead       ;
	rts						;--
	
:	ldy PpuBuffer, x		; Read in dst PPUADDR
	inx						; Y = PPUADDR_LO
	lda PpuBuffer, x		; A = PPUADDR_HI
	inx						;--
	
	sty n0					; Save PPUADDR_LO in n0
	sta n1					; Save the write mode bit in n1
	and #$3F				; .. and then set PPUADDR to the current target.
	sta PPUADDR				;
	sty PPUADDR 			;--
	
	lda PpuCtrlShadow		; Set PPUCTRL write mode based on n1.bit7
	bit n1					; .. this is the vertical write mode flag.
	bvs @attribMode			; .. branch if the attrib write mode is active.
	bpl @horizontalMode		;
	ora #%00000100          ; (1) Vertical Mode
	bne @setPpuCtrl         ;
@horizontalMode:            ;
	and #%11111011          ; (0) Horizontal Mode
@setPpuCtrl:                ;
	sta PPUCTRL             ;--
	
	lda PpuBuffer, x		; Y = Data Length
	inx						;
	sta n2                  ; .. store RLE flag in n2 ..
	and #$7F                ;
	tay	                    ; --
	
	bit n2					; RAW or RLE mode?
	bmi @rleWrite			;--

@rawWrite:					; Perform RAW write to PPUDATA
	lda PpuBuffer, x		; 
	sta PPUDATA             ;
	inx                     ;
	dey                     ;
	bne @rawWrite           ;
	beq @checkBuffer        ;--

@rleWrite:					; Perform RLE write to PPUDATA
	lda PpuBuffer, x        ;
	inx                     ;
:	sta PPUDATA             ;
	dey                     ;
	bne :-                  ;
	beq @checkBuffer        ;--

@attribMode:
	and #%11111011 			; A = PpuCtrlShadow
	sta PPUCTRL				; Set Horizontal mode.
	
	lda PpuBuffer, x		; Fetch RLE / Data Length byte
	inx						;
	sta n2					; .. RLE flag stored in n2
	and #$7F				;
	tay						;--
	
	bit n2					; Check for RLE write mode. 
	bmi @attribRleWrite		;-- [ RIP Bug #210616 ]
	
@attribRawWrite:
	lda n1					; Write n1 to n3 to preserve overflow flag.
	sta n3                  ;--

	and #$3F				; n1 = PPUADDR_HI Pointer
	sta n1					;--
	
	clc						; Clear carry for saftey.
:	lda PpuBuffer, x		; Write one byte.
	sta PPUDATA				;
	inx						;--
	dey						; Check for completion
	beq @checkBuffer		;--
	
	bit n3 					; Check for vertical mode.
	bvc :-					; Continue if horizontal mode.
	
	lda n1					; Move Attrib Addr pointer forward 8 bytes.
	sta PPUADDR				;
	lda #$08				;
	adc n0					;
	sta n0                  ;
	sta PPUADDR             ;--
	bne :-					; Always branch, A = $C0 - $F8
	jmp @checkBuffer
	
@attribRleWrite:
	lda n1					; Write n1 to n3 to preserve overflow flag.
	sta n3                  ;--
	and #$3F				; n1 = PPUADDR_HI Pointer
	sta n1					;--
	
	clc						; Clear carry for saftey.
	lda PpuBuffer, x		; .. save RLE byte in n2
	inx						; 
	sta n2					;--
	
:	sta PPUDATA				;
	dey						; Check for completion
	bne @checkMode			;--
	jmp @checkBuffer 
	
@checkMode:					; Continue if horizontal mode.
	bit n3
	bvc :-					;--
	
	lda n1					; Move Attrib Addr pointer forward 8 bytes.
	sta PPUADDR				;
	lda #$08				;
	adc n0					;
	sta n0                  ;
	sta PPUADDR             ;--
	
	lda n2					; Restore RLE byte from n2
	
	bne :-					; Always branch
	beq :-

