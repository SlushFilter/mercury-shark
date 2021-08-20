; m_stageload.asm
; Stage Load - Routine to load up a stage, assumes players have just entered the
; 			   stage.
; Loaded into Bank7

.include "defines.inc"
.include "ram-map.inc"
.autoimport +

DMARKER "### _StageLoad ###"
.scope NS_STAGELOAD		; Set NS_STAGELOAD namespace
.export F_GetStageIndex

; StageHeader Offsets
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
STAGE_LENGTH		= $00	; Length, in screens, of the current stage.
START_SCREEN		= $01
SPAWN1_XY			= $02
SPAWN2_XY			= $03
CHR_5				= $04	; CHR Page to load for slot 6
CHR_6				= $05	; CHR Page to load for slot 7
CHR_7				= $06	; CHR Page to load for slot 8
BG_PAL_0			= $07	; Background Palette 0	
BG_PAL_1			= $08   ; Background Palette 1
BG_PAL_2			= $09	; Background Palette 2
BG_PAL_3			= $0A	; Background Palette 3

OFS_STAGEMAP		= $0B	; Offset of StageMap
OFS_TILEMAPS		= $0D	; Offset of Tile Maps
OFS_ATTRIBS			= $0F	; Offset of Attribute Maps
OFS_BLOCKMAPS		= $11	; Offset of Block Maps
OFS_METATILES		= $13	; Offset of MetaTiles
OFS_TILES			= $15	; Offset of Tiles


_StageLoad:
	jsr F_ClearProtected

	ldx #$00
	stx PpuBufferWrite
	stx PpuBufferRead
	
	jsr F_LoadHeader
	jsr F_DisableGfx
	jsr F_LoadTileMap
	jsr F_EnableGfx
	
	lda #M_STAGEINTRO	
	sta ProgramMode
	rts

; =============================================================================
; F_GetStageIndex
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Returns
; Y : Word index derived from Level and Stage values, used for looking up stage
;     pointers from a pointer table.
; =============================================================================
F_GetStageIndex:
	clc							; Calcualte Source Pointer Offset
	lda Level                   ;
	sta v0                      ;
	asl                         ;
	asl                         ;
	adc v0                      ;
	adc Stage                   ;
	asl                         ;
	tay                         ;--
	rts
	
; =============================================================================
; F_GetStageHeaderPtr
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Looks at the current Level / Stage pair and returns a pointer to that stage's
; header.
; =============================================================================
F_GetStageHeaderPtr:
	lda #BANK_STAGEDATA
	jsr F_SelectBank
	
	jsr F_GetStageIndex
	lda DP_StageData, y
	ldx DP_StageData + 1, y
	sta StageHeaderPtr
	stx StageHeaderPtr + 1
	rts

; =============================================================================
; F_LoadHeader
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Loads the Stage Header in from the Stage Data bank.
; =============================================================================
F_LoadHeader:
	jsr F_GetStageHeaderPtr
	lda #COLOR_BLACK		; TODO: Removeme for Debugging Only!
	sta BgColor     		;

	jsr F_DisableGfx 		; Disable Background and Foreground
	
	lda #BANK_STAGEDATA		; Select the STAGEDATA bank
	jsr F_SelectBank		; --
	ldy #STAGE_LENGTH		; Load in the Stage length.
	lda (StageHeaderPtr), y ;
	sta StageLength			;
	ldy #CHR_5				; Load BG Tileset for Slot 6	
	lda (StageHeaderPtr), y ;
	sta v0, y               ;
	ldy #CHR_6				; Load BG Tileset for Slot 7
	lda (StageHeaderPtr), y ;
	sta v0, y               ;
	ldy #CHR_7				; Load BG Tileset for Slot 8
	lda (StageHeaderPtr), y ;
	sta v0, y               ;--
	
	ldy #BG_PAL_0			; Load BG Palette 0
	lda (StageHeaderPtr), y	;
	sta v0, y				;
	ldy #BG_PAL_1			; Load BG Palette 1
	lda (StageHeaderPtr), y	;
	sta v0, y               ;
	ldy #BG_PAL_2			; Load BG Palette 2
	lda (StageHeaderPtr), y	;
	sta v0, y               ;
	ldy #BG_PAL_3			; Load BG Palette 3
	lda (StageHeaderPtr), y	;
	sta v0, y               ;--
	
	ldy #CHR_5				; Load in Tilesets
	lda v0, y               ;
	tay                     ;
	lda #$05                ; .. Slot 5
	jsr F_LoadTileset       ; 
							;
	ldy #CHR_6              ;
	lda v0, y               ;
	tay                     ;
	lda #$06                ; .. Slot 6
	jsr F_LoadTileset       ;
							;
	ldy #CHR_7              ;
	lda v0, y               ;
	tay                     ;
	lda #$07                ; .. Slot 7
	jsr F_LoadTileset       ; --
	
	ldy #BG_PAL_0			; Load in Palettes
	lda v0, y               ;
	tay                     ;
	lda #$00                ; .. Slot 0
	jsr F_LoadPalette       ;
	jsr F_WaitVblank		; Wait for Palette Data to Load

	ldy #BG_PAL_1           ;
	lda v0, y               ;
	tay                     ;
	lda #$01                ; .. Slot 1
	jsr F_LoadPalette       ;
	jsr F_WaitVblank		; Wait for Palette Data to Load

	ldy #BG_PAL_2           ;
	lda v0, y               ;
	tay                     ;
	lda #$02                ; .. Slot 2
	jsr F_LoadPalette       ;
	jsr F_WaitVblank		; Wait for Palette Data to Load

	ldy #BG_PAL_3           ;
	lda v0, y               ;
	tay                     ;
	lda #$03                ; .. Slot 3
	jsr F_LoadPalette       ; --
	jsr F_WaitVblank		; Wait for Palette Data to Load
	
	lda #BANK_NTPAL
	jsr F_SelectBank

	lda #BANK_STAGEDATA
	jsr F_SelectBank

	ldx #$00				; Load in pointers to :
	ldy #OFS_STAGEMAP		; PtrStageMap
:	lda (StageHeaderPtr), y ; PtrScreenData
	sta BaseStageMap, x     ; PtrMetaCols
	iny                     ; PtrTileData
	inx                     ; PtrAttribCols
	cpx #$0C                ;
	bne :-                  ;--
	lda #$00				;
	sta Scroll_X			;
	sta Scroll_Y			;
	sta LockLeft			;
	lda StageLength			;
	sta LockRight			;
	lda #$01				;
	sta Scroll_XHI			;

	rts

PtrTileMap		= m0
PtrTileMap_HI	= m1
PtrAttribs		= m2
PtrAttribs_HI	= m3
PtrMetaTiles	= m4
PtrMetaTiles_HI = m5
PtrTiles		= m6
PtrTiles_HI		= m7
ScreenColumn	= m8
TileMap_Idx		= m9
Tile_Idx		= mA
MetaTile_Idx    = mB

; =============================================================================
; F_LoadTileMap
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Loads the current TileMap.
; =============================================================================
F_LoadTileMap:
	lda #BANK_STAGEDATA		; Loading up data from the stage data bank.
	jsr F_SelectBank		; --
	
	jsr F_GetPtrTileMap		; Get the offsetted pointer to the current TileMap
	ldy #$00				; Reset the ScreenColumn indexer.
	sty ScreenColumn		;
	sty TileMap_Idx			;--
	
@outerLoop:
	lda BaseMetaTiles		; Reset PtrMetaTiles
	sta PtrMetaTiles        ;
	lda BaseMetaTiles + 1   ;
	sta PtrMetaTiles_HI     ;--

	lda BaseTiles			; Reset PtrTiles
	sta PtrTiles            ; 
	lda BaseTiles + 1       ;
	sta PtrTiles_HI         ;--
	
	lda ScreenColumn		; Adjust PtrMetaTile if we need the right half of 
	and #$02                ; .. the metatile for this pass.
	beq :+                  ;
	inc PtrMetaTiles        ;
	bne :+                  ;
	inc PtrMetaTiles + 1    ;--
	
:	lda ScreenColumn		; Adjust PtrTiles if we need the right half of
	and #$01				; .. the tile for this pass.
	beq :+                  ;
	inc PtrTiles            ;
	bne :+                  ;
	inc PtrTiles_HI         ;--

:	lda ScreenColumn		; Update TileMap Index
	lsr                     ;
	lsr                     ;
	sta TileMap_Idx         ;--
	
	ldx PpuBufferWrite      ;
	lda ScreenColumn		; Set the target VRam Address
	ldy #$A0                ; .. prep to write in vertical mode to $2000
	jsr F_PpuBufferPushWord ;--
	
	lda #$1C				; 28 Bytes of data to push.
	sta PpuBuffer, x		;
	inx						;--
	
@metaLoop:	
	
	ldy TileMap_Idx			; Fetch MetaTile
	lda (PtrTileMap), y		;
	asl						;
	asl						;
	tay						;
	sta MetaTile_Idx		;--
	
	lda (PtrMetaTiles), y	; Fetch the Top Tile Index
	asl						;
	asl						;
	tay 	                ;--

	lda (PtrTiles), y		; Fetch top tile and write to PpuBuffer 
	sta PpuBuffer, x		;
	inx                     ;
	iny                     ;
	iny                     ;
	lda (PtrTiles), y    	;
	sta PpuBuffer, x        ;
	inx                     ;--
	
	ldy MetaTile_Idx		; Fetch the Bottom Tile Index
	iny						;
	iny                     ;
	lda (PtrMetaTiles), y   ;
	asl						;
	asl                     ;
	tay                     ;--

	lda (PtrTiles), y		; Fetch bottom tile and write to PpuBuffer 
	sta PpuBuffer, x		;
	inx                     ;
	iny                     ;
	iny                     ;
	lda (PtrTiles), y    	;
	sta PpuBuffer, x        ;
	inx                     ;--
	
	clc						; Step forward a row and continue if we're not out
	lda TileMap_Idx         ; of bounds yet.
	adc #$08                ;
	sta TileMap_Idx         ;
	cmp #$38                ;
	bcc @metaLoop			;--
	
	lda ScreenColumn		; Check for attribute column write.
	and #$03				;
	bne @commitBuffer		;--
	
	lda ScreenColumn		; Set PPUADDR $23xx
	lsr						;
	lsr						;
	ora #$C0				;
	ldy #$E3				;
	jsr F_PpuBufferPushWord	;--
	
	lda #$07				; 7 bytes of raw data
	sta PpuBuffer, x		;
	inx						;--
	
	lda ScreenColumn		; Find index into attrib table
	lsr						; 
	lsr                     ;
	tay                     ;--
						
:	lda (PtrAttribs), y  	; Write Attributes to Attrib Table 
	sta PpuBuffer, x        ;
	inx                     ;
	tya						;
	adc #$08				;
	tay 					;
	cpy #$38                ;
	bcc :-                  ;
	
@commitBuffer:	
	stx PpuBufferWrite		; Commit the write.
	jsr F_WaitVblank		; Wait for the column to render.
 	clc						; Advance to next screen column.
	lda ScreenColumn        ;
	adc #$01                ;
	sta ScreenColumn		;
	cmp #$20                ;
	beq :+
	jmp @outerLoop          ;--
	
:	rts


F_GetPtrTileMap:
	clc 
	ldy #$00				; Initialize (word) v2 
	sty v0					;
	sty v1					;--
	
	ldy #$00				; Calculate pointer offset
	lda (BaseStageMap), y   ; .. (word) v0 = Index * 64   
	and #$0F				;
	lsr 					;
	ror v0          		;                                               
	lsr                     ;                                               
	ror v1          		;                                               
	sta v0       			;--
	
	sta v2					; Make a copy of this offset.
	lda v1                  ; .. (word)v2 = Index * 64
	sta v3                  ;--
	
	lda BaseTileMap			; 16 bit maths to find the MetaTile 0, 0
	ldx BaseTileMap + 1   	; pointer.
	jsr F_Add16             ;
	sta PtrTileMap			;
	stx PtrTileMap_HI		;--
	
	lda v2					; Copy the pointer offset back to (word)v0
	sta v0                  ;
	lda v3                  ;
	sta v1                  ;--
	
	lda BaseAttribs			; 16 bit maths to find the Attribs 0, 0
	ldx BaseAttribs + 1     ;
	jsr F_Add16             ;
	sta PtrAttribs          ;
	stx PtrAttribs_HI       ;--
	rts 

.endscope
