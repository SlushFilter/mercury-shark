; m_stagerun.asm
; Stage Run - Main gameplay routine.
; 			  
; Loaded into Bank7
.include "defines.inc"
.include "ram-map.inc"
.autoimport +

DMARKER "### _StageRun ###"
.scope NS_STAGERUN		; Set NS_STAGERUN namespace

_StageRun:
	lda #BANK_GAME		; Get us over to bank 3 for entity processing.
	jsr F_SelectBank    ; --
	jsr F_EntityLogic	; Process entities.
	jsr F_ParticleLogic	; Process particles.
	
	lda #BANK_STAGEDATA		; Get us over to StageData for tile loading.
	jsr F_SelectBank    	; --
	jsr F_StageScroll		; Update Stage Scroll Registers

	lda #BANK_SPRITES		; Get us over to StageData for tile loading.
	jsr F_SelectBank    	; --

	jsr F_DrawSprites
	jsr F_DrawParticles
	jsr F_WaitVblank	; Wait a frame.
	
	rts


F_StageScroll:
	ldy StageScreenIdx		;
	lda (BaseStageMap), y	;
	bpl :+
	sty LockLeft
:	and #$40
	beq :+
	sty LockRight
:	sec						; Calculate screen scroll.
	lda Ent_P1_X			;
	sbc #$80				;
	sta v0					; .. v0 = Scroll_X
	lda Ent_P1_Screen		;
	sbc #$00				;
	sta StageScreenIdx		;
	and #$01				; .. v1 = Scroll_XHI
	sta v1					; --
	
	clc 
	lda StageScreenIdx		; Check for Left Scroll Lock
	adc #$01				;
	cmp LockLeft			;
	bne @lockRight			; --
	lda LockLeft			; Lock the screen if we're as far left as we can go
	sta StageScreenIdx		;
	and #$01				;
	sta v1                  ; --
	lda #$00				;
	sta v0					;

@lockRight:	
	sec 					; Check for Right Scroll Lock
	lda StageScreenIdx		;
	cmp LockRight         	;
	bne @updateScroll       ; --
	lda LockRight         	; Lock the screen if we're as far right as we can go
	sta StageScreenIdx      ;
	and #$01				;
	sta v1					;
	lda #$00                ;
	sta v0                  ; --
	
@updateScroll:
	lda v0					; Update scroll registers.
	sta Scroll_X            ;
	lda v1                  ;
	sta Scroll_XHI          ; --
	
@checkSeam:
	lda Scroll_X			; Check to see if the load seam moved. 
	lsr                 	;
	lsr                 	;
	lsr                 	;
	and #$1F				;
	tax 					; .. save the calculated value in X
	cmp SeamColumn			;
	beq @return				; --
		
	clc						; Deterimine if the screen scrolled right or left.
	adc #$01            	;
	and #$1F				;
	cmp SeamColumn			;
	beq @scrollLeft     	; --
	
@scrollRight:
	clc						; Advance the SeamColumn
	lda SeamColumn			;
	adc #$01            	;
	and #$1F            	;
	sta SeamColumn      	; --
	
	ldy StageScreenIdx		; Write to the other nametable.
	iny						;
	clc						;
	lda SeamColumn      	; .. Right hand side of the screen.
	adc #$20            	;
	and #$1F            	;
	tax						;
	jsr F_LoadTileColumn	; --
	
	lda SeamColumn			; Check for Attribute table update.
	and #$03                ;
	bne @return             ; --
	
	clc						; Setup for Attribute write.
	lda SeamColumn          ; .. calculate target attribute column.
	adc #$20				;
	lsr                     ;
	lsr                     ;
	and #$07                ;
	tax                     ; --
	
	clc						; Set target Screen
	lda StageScreenIdx		;
	adc #$01				;
	tay                     ;
	jsr F_LoadAttribColumn	; --
	
	jmp @return
	
@scrollLeft:
	sec						; Decrement the seam column.
	lda SeamColumn          ;
	sbc #$01                ;
	and #$1F                ;
	sta SeamColumn          ; --
	
	tax                     ; Write the screen column.
	ldy StageScreenIdx      ;
	jsr F_LoadTileColumn    ; --
	
	lda SeamColumn			; Check for Attribute table update.
	and #$03                ;
	eor #$03                ;
	bne @return             ; --
	
	lda SeamColumn			; Setup for Attribute write.
	lsr						; .. calculate target attribute column.
	lsr						;
	and #$07                ;
	tax                     ; --
	
	lda StageScreenIdx		; Set target screen and write the attributes.
	tay						;
	jsr F_LoadAttribColumn	; --
	
@return:	
	rts

PtrTileMap   = m0
PtrAttribs   = m2
PtrMetaTiles = m4
PtrTiles	 = m6

TileMap_Idx  = m8
MetaTile_Idx = m9
ScreenColumn = mA 

; Y = Screen
; X = Column (0 - 31)

F_LoadTileColumn:
	stx ScreenColumn		;
	clc						; Fetch the pointer offset to tilemap and attrib 
	lda #$00                ; .. map. Store it in v0.
	sta v0         			; .. Offset is Screen * 64
	lda (BaseStageMap), y   ;
	and #$0F				;
	tax						;
	lda D_Mult64_LO, x      ;
	sta v0                  ;
	lda D_Mult64_HI, x      ;
	sta v1                  ; --
	
	clc						; Add on the Source Data Column offset from LeftSeam
	lda ScreenColumn       	;
	lsr						;
	lsr						;
	and #$07				;
	clc						;
	adc v0                  ;
	sta v0                  ;
	lda v1                  ;
	adc #$00                ;
	sta v1                  ; --
	
	lda v0					; Copy it over to v2 for use in next calculation.
	sta v2					;
	lda v1					;
	sta v3					; --
	
	lda BaseTileMap			; Calculate PtrTileMap
	ldx BaseTileMap + 1     ;
	jsr F_Add16             ;
	sta PtrTileMap          ;
	stx PtrTileMap + 1      ; --
	
	lda v2					; Restore previous offset calc.
	sta v0					;
	lda v3                  ;
	sta v1                  ; --
	
	lda BaseAttribs			; Calculate PtrAttribMap
	ldx BaseAttribs + 1   	;
	jsr F_Add16             ;
	sta PtrAttribs        	;
	stx PtrAttribs + 1    	; --
	
	ldx BaseMetaTiles		; Calculate MetaTile Offset
	lda ScreenColumn		;
	and #$02                ;
	beq :+                  ;
	inx                     ;
:	stx PtrMetaTiles        ;
	lda BaseMetaTiles + 1   ;
	sta PtrMetaTiles + 1    ; --
	
	ldx BaseTiles			; Calculate Tile Offset
	lda ScreenColumn        ;
	and #$01                ;
	beq :+                  ;
	inx                     ;
:	stx PtrTiles            ;
	lda BaseTiles + 1       ;
	sta PtrTiles + 1        ; --
	
	ldx PpuBufferWrite		; Start off with Target PPU Address and data length.
	tya						; 
	and #$01				;
	bne :+                  ; 
	ldy #$A0                ; .. NameTable $20xx ..  vertical write.
	bne :++                 ;
:	ldy #$A4                ; .. NameTable $24xx .. vertical write.
:	lda ScreenColumn        ;
	and #$1F                ; .. NameTable $xxAA
	jsr F_PpuBufferPushWord ; .. Write the target VRam Address
	lda #$1C				; .. Prep to write 28 bytes of data.
	sta PpuBuffer, x		; 
	inx                     ; --
	
	lda #$00				; Start at the first row.
	sta TileMap_Idx			; --
	
@metaLoop:	
	ldy TileMap_Idx			; Fetch the MetaTile Index
	lda (PtrTileMap), y     ;
	asl						;
	asl						;
	sta MetaTile_Idx        ;
	tay						; --
	
	lda (PtrMetaTiles), y	; Fetch Top of MetaTile
	asl						;
	asl                     ;
	tay                     ; --
	
	lda (PtrTiles), y		; Fetch Write Bottom of Tile
	sta PpuBuffer, x		; --
	
	inx						; Fetch Write Bottom of Tile
	iny						;
	iny						;
	lda (PtrTiles), y		;
	sta PpuBuffer, x		;
	inx						; --
	
	lda MetaTile_Idx		; Fetch Bottom of Metatile
	tay						;
	iny                     ;
	iny                     ;
	lda (PtrMetaTiles), y	;
	asl                     ;
	asl                     ;
	tay                     ; --

	lda (PtrTiles), y		; Fetch Write Top of Tile
	sta PpuBuffer, x		; --
	
	inx						; Fetch Write Bottom of Tile
	iny						;
	iny                     ;
	lda (PtrTiles), y       ;
	sta PpuBuffer, x        ;
	inx                     ; --
	
	lda TileMap_Idx			; Advance to next row of TileMap 
	adc #$08                ;
	sta TileMap_Idx         ;
	cmp #$38                ; .. exit the loop if we're done.
	bcc @metaLoop			;--
	stx PpuBufferWrite
	rts
	
; Y = Screen
; X = Attribute Column (0 - 7)
F_LoadAttribColumn:
	stx ScreenColumn
	lda (BaseStageMap), y	; Fetch Screen Number
	and #$0F                ;
	tax                     ; --
	lda D_Mult64_LO, x		; Calculate pointer to Attrib data.
	sta v0                  ;
	lda D_Mult64_HI, x      ;
	sta v1                  ;
	clc                     ;
	ldx ScreenColumn		;
	txa                     ;
	adc BaseAttribs         ;
	adc v0                  ;
	sta v0                  ;
	lda BaseAttribs + 1     ;
	adc v1                  ;
	sta v1                  ; --
	
	txa						; Target Vram Address Low
	ora #$C0                ;
	ldx PpuBufferWrite		;
	sta PpuBuffer, x        ;
	inx						; --
	tya						; Target Vram Address Hi
	and #$01                ;
	tay                     ;
	lda D_AttribAddr_HI, y  ;
	sta PpuBuffer, x        ;
	inx						; --
	lda #$07				; Data Length is 7 bytes.
	sta PpuBuffer, x   		;
	inx                     ; --
	
	ldy #$00				; Read Offset
@attribLoop:
	lda (v0), y				; Read in the attribute
	sta PpuBuffer, x        ; .. push it onto the PpuBuffer
	inx                     ;
	tya                     ;
	adc #$08                ; .. advance the read index
	tay                     ;
	cpy #$38                ; .. and see if we're done.
	bcc @attribLoop         ;
	stx PpuBufferWrite      ;
	
	rts 					; All Done!

D_AttribAddr_HI:
	.byte $63, $67
.endscope