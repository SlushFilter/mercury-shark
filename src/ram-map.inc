; =============================================================================
; Memory Map     
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; [$0000 : $000F] Volitile, Variables (v0 - vF) and (nptr0 - nptrE)
; [$0010 : $001F] Volitile, NMI General use vars (n0 - nF) and (nptr0 - nptrE)
; [$0020 : $003F] Entity Staging for Processing
; [$0040 : $0068] ** Unused **
; [$0069 : $0069] Reserved for UltraMinimalist
; [$006A : $008F] ** Unused **
; [$0090 : $009F] Stage Data Pointers
; [$00A0 : $00BF] Game Mode running vars (during stages)
; [$00C0 : $00CF] ProgramMode Vars (m0 - mF)
; [$00D0 : $00DF] System Globals
; [$00E0 : $00EF] PPU Globals
; [$0100 : $01FF] System Stack
; [$0200 : $02FF] OAM Ram
; [$0300 : $03FF] FamiTone5 Driver
; [$0400 : $04FF] Entity Data 
; [$0500 : $057F] Particle Data (16 slots)
; [$0580 : $06FF] ** Unused **
; [$0700 : $07FF] Ppu Command Buffer
; =============================================================================

.ifndef MEMORY_MAP

; [$0000 : $000F] Volitile, General use vars (v0 - vF) and (nptr0 - nptrE)
; -----------------------------------------------------------------------------
	v0				= $00
	v1				= $01
	v2				= $02
	v3				= $03
	v4				= $04
	v5				= $05
	v6				= $06
	v7				= $07
	v8				= $08
	v9				= $09
	vA				= $0A
	vB				= $0B
	vC				= $0C
	vD				= $0D
	vE				= $0E
	vF				= $0F
	; Unionized volitile pointers.
	ptr0			:= $00
	ptr2			:= $02
	ptr4			:= $04
	ptr6			:= $06
	ptr8			:= $08
	ptrA			:= $0A
	ptrC			:= $0C
	ptrE			:= $0E

; [$0010 : $001F] Volitile, NMI General use vars (n0 - nF) and (nptr0 - nptrE)
; -----------------------------------------------------------------------------
	n0				= $10
	n1				= $11
	n2				= $12
	n3				= $13
	n4				= $14
	n5				= $15
	n6				= $16
	n7				= $17
	n8				= $18
	n9				= $19
	nA				= $1A
	nB				= $1B
	nC				= $1C
	nD				= $1D
	nE				= $1E
	nF				= $1F
	; Unionized NMI volitile pointers.
	nptr0			:= $10
	nptr2			:= $12
	nptr4			:= $14
	nptr6			:= $16
	nptr8			:= $18
	nptrA			:= $1A
	nptrC			:= $1C
	nptrE			:= $1E

; [$0020 : $003F] Volitile, NMI General use vars (n0 - nF) and (nptr0 - nptrE)
; -----------------------------------------------------------------------------
	Ent				= $20
	Ent_Flags		= $20  ; Entity Flags AF.. ....
						   ;	A : ENT_ACTIVE (0)Inactive (1)Active
						   ;    F : ENT_FACELEFT (0)Facing Right (1)Facing Left
						   
	Ent_Screen		= $21  ; Current Screen Index this entity is on.(Low Nybble)
	
	Ent_SubX		= $22  ; X Subpixel Coordinate
	Ent_X			= $23  ; X Coordinate
	Ent_SubY		= $24  ; Y Subpixel Coordinate
	Ent_Y	       	= $25  ; Y Coordinate
	
	Ent_Vel_SubX	= $26  ; Subpixel X Velocity
	Ent_Vel_X		= $27  ; X Velocity
	Ent_Vel_SubY	= $28  ; Subpixel Y Velocity
	Ent_Vel_Y		= $29  ; Y Velocity
		
	Ent_Type		= $2A  ; Type of entity, determines what logic to run.
	Ent_MoveFlags	= $2B  ; Movement Flags LUHV G...
						   ; 	L : MOVE_LEFT  - Ent is moving left (negative X)
						   ; 	U : MOVE_UP    - Ent is Moving Up (negative Y)
						   ; 	H : MOVE_HMOVE - Ent is moving horizontally.
						   ; 	V : MOVE_VMOVE - Ent is moving vertically.
						   ; 	G : MOVE_GROUNDED - Ent is on the ground.

	Ent_Width		= $2C  ; Entity Half Width
	Ent_Height		= $2D  ; Entity Full Height
	Ent_Block		= $2E  ; Current Block Occupied
	Ent_AiStage		= $2F  ; Current AI Stage
	
	Ent_MoveState	= $30  ; Movement state. Eg; falling, walking, flying ..
	Ent_ActionState	= $31  ; Attack state. Currently attacking, and what state.
	Ent_Status		= $32  ; Entity Status, Normal, Stunned, Dying, etc ..
	
	Ent_13			= $33  ; Entity Auxilarry value
	Ent_14			= $34  ; Entity Auxillary Value 
	Ent_15			= $35  ; Entity Auxillary Value 
	Ent_IFrames		= $36  ; Entity IFrame Counter
	Ent_Health		= $37  ; Entity Health
	Ent_Damaged		= $38  ; Value indicating the damage received for the frame.
	
	Ent_ChrBase		= $39  ; Base CHR tile index, usually $00, $40, $80, or $C0
	Ent_AnimIndex	= $3A  ; Animation index in the D_AnimPtr table.
	Ent_FrameCount	= $3B  ; High Nybble is Anim Length,	Low Nybble is Index.
	Ent_FrameTimer  = $3C  ; High Nybble is Frame Duration, Low Nybble is Count.
	Ent_FrameMod	= $3D  ; This value is XOR'd with the Frame Attrib byte.
	Ent_FramePtr	= $3E  ; Frame Pointer (word)
	Ent_FramePtr_LO	= $3E  ; Frame Pointer Low Byte
	Ent_FramePtr_HI	= $3F  ; Frame Pointer Hi Byte

; [$0069 : $0069] UltraMinimalist's Byte
; -----------------------------------------------------------------------------
	UltraMinimalist_Byte = $69
	
; [$0090 : $009F] Stage Data Pointers
; -----------------------------------------------------------------------------
	StageHeaderPtr	= $92	; Pointer to stage header data.
	BaseStageMap	= $94	; (word) Pointer to the current StageMap
	BaseTileMap		= $96	; (word) Pointer to TileMap
	BaseAttribs		= $98	; (word) Pointer to Attribute Columns
	BaseBlockMap	= $9A	; (word) Pointer to BlockMaps
	BaseMetaTiles	= $9C	; (word) Pointer to MetaTiles
	BaseTiles		= $9E	; (word) Pointer to Tile Data

; [$00A0 : $00BF] Game Mode running vars (during stages)
; -----------------------------------------------------------------------------
	PlayerChars		= $A0	; Flags for the currently running game.
							; Q.BB P.AA
							; P : Player 1 Active Flag (0) Inactive (1) Active
							; Q : Player 2 Active Flag (0) Inactive (1) Active
							; AA : Player 1 Character seleciton. (0-3)
							; BB : Player 2 Character seleciton. (0-3)
	Level			= $A8	; Current Level
	Stage			= $A9	; Current Stage
	StageLength		= $AA 	; Number of screens on this stage.
	LockLeft		= $AB	; Left Screen Index Lock
	LockRight		= $AC	; Right Screen Index Lock
	SeamColumn  	= $AD	; Current Scroll Value, 0 - 31 (Left edge of screen)
	StageScreenIdx	= $AE	; Current Screen Index on left edge.
	Ent_Idx			= $AF	; Currently Selected Entity

; [$00C0 : $00CF] Program Mode Vars - Becomes Volitile after ProgramMode change.
; -----------------------------------------------------------------------------
	m0				= $C0
	m1              = $C1
	m2              = $C2
	m3              = $C3
	m4              = $C4
	m5              = $C5
	m6              = $C6
	m7              = $C7
	m8              = $C8
	m9              = $C9
	mA              = $CA
	mB              = $CB
	mC              = $CC
	mD              = $CD
	mE              = $CE
	mF              = $CF
	
; [$00D0 : $00DF] System Globals
; -----------------------------------------------------------------------------
	Joy1_State		= $D0	; Joypad 1 Button State.
	Joy2_State		= $D1   ; Joypad 2 Button State.
	Joy1_Held		= $D2   ; Joypad 1 Held Button State.
	Joy2_Held		= $D3   ; Joypad 2 Held Button State.
	Joy1_Delta		= $D4	; Joypad 1 Difference since last frame.
	Joy2_Delta		= $D5	; Joypad 2 Difference since last frame.
	PrgBank			= $D6	; Current PRG Bank
	LastBank		= $D7	; Last PRG Bank
	ProgramMode		= $D8	; The current program mode.
							; High Nybble is program mode, Low Nybble is SubMode
							
	SFX_0			= $D9	; Sound Effect 0 Queue
	SFX_1           = $DA   ; Sound Effect 1 Queue
	SFX_2           = $DB   ; Sound Effect 2 Queue
	SFX_3           = $DC   ; Sound Effect 3 Queue
	
	BGM				= $DD	; Current BGM ($00 = No BGM, $FF = Stop Music)
	
	Rng				= $DF	; Rng / FrameCounter

	
; [$00E0 : $00EF] PPU Globals
; -----------------------------------------------------------------------------
	PpuBufferRead	= $E0	; Ppu Data Buffer Read Head
	PpuBufferWrite	= $E1	; Ppu Data Buffer Write Head
	BgColor			= $E2	; Current global background color
	PpuCtrlShadow 	= $E3	; Copy of the last value written to PPUCTRL
	PpuMaskShadow	= $E4	; Copy of the last value written to PPUMASK
	OamIndex		= $E5	; Current index into OAM ram for writing.
	Scroll_Y		= $E6	; Vertical Scroll (8bit)
	Scroll_SubX		= $E7
	Scroll_X		= $E8	; Horizontal Scroll(16bit)
	Scroll_XHI		= $E9	; Base nametable select.
	
	PpuOverride		= $EA	; PPU Override Flags
							; CM.. ....
	
	ScreenColumn	= $EB	; Current Screen Write Column for scrolling updates.
							; Measured at a precision of 1 column per 8 pixels.
							; Range is 0-31(Screen 0) 32-63 (Screen 1)
	
; [$0200 : $02FF] OAM Ram
; -----------------------------------------------------------------------------
	OamRam			= $200	; OAM Ram

; [$0400 : $04FF] Entity Data 
; -----------------------------------------------------------------------------
	Ent_Data		= $400	; Base Entity Data Pointer.
	Ent_P1_Flags	= $400
	Ent_P1_Screen	= $401	; Player 1 Absolute Screen Index
	Ent_P1_X		= $403	; Player 1 Absolute X
	Ent_P1_Y		= $405	; Player 1 Absolute Y
	Ent_P1_Width    = $40C	; Player 1 Half Width
	Ent_P1_Height	= $40D	; Player 1 Full Height
	Ent_P1_Block	= $40E	; Player 1 last occupied tile
	Ent_P1_IFrames	= $416	; Player 1 IFrame counter
	Ent_P1_Damaged  = $418	; Player 1 Damaged Type

	Ent_P2_Flags	= $420	;
	Ent_P2_Block	= $42E	; Player 2 last occupied tile

	Ent_P2_Screen	= $421	; Player 2 Absolute Screen Index
	Ent_P2_X		= $423	; Player 2 Absolute X
	Ent_P2_Y		= $425	; Player 2 Absolute Y
	Ent_P2_Width    = $42C	; Player 2 Half Width
	Ent_P2_Height	= $42D	; Player 2 Full Height
	Ent_P2_IFrames	= $436	; Player 2 IFrame counter
	Ent_P2_Damaged  = $438	; Player 2 Damaged Type


	EntData_X		= $403  ; X Coordinate
	EntData_Y     	= $405  ; Y Coordinate
	EntData_Width	= $40C	; Halfwidth
	EntData_Height	= $40D	; Fullheight
	EntData_Damaged = $418	; Incoming Damage Type
	EntData_FrameMod = $41D	 ; Sprite Frame Mod XOR
; [$0500 : $059F] Particle Data (16 slots, parallel arrays)
; -----------------------------------------------------------------------------
	Par_Type		= $500	; Type
	Par_Tile		= $510  ; Tile
	Par_Attr		= $520	; Sprite Attribute
	Par_X 			= $530  ; X
	Par_SubX        = $540  ; SubX
	Par_Y 			= $550  ; Y
	Par_SubY        = $560  ; SubY
	Par_VX			= $570  ; Velocity X (4.4 Fixed point)
	Par_VY			= $580	; Velocity Y (4.4 Fixed point)
	Par_Var			= $590	; Auxillary Value :)
	
; [$06E0 : $06FF] Palette Buffer
; -----------------------------------------------------------------------------
	PaletteBuffer	= $6E0	; Buffer area for palette fading in and out.
	
; [$0700 : $07FF] Ppu Command Buffer
; -----------------------------------------------------------------------------
	PpuBuffer		= $700	; Ppu Command Buffer : 256 byte ring buffer


MEMORY_MAP = 1
.endif