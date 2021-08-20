; m_menu.asm
; Main Menu Routine
; Loaded into Bank7
.include "defines.inc"
.include "ram-map.inc"
.autoimport +

DMARKER "### _MainMenu ###"
.scope NS_MENU			; Set NS_MENU namespace


; Main Menu Local Defines
; ------------------------------------------------------------------------------
MENU_PALETTE	= $00

SM_SPLASH 		= $01
SM_TITLE		= $02
SM_STORY		= $03
SM_PLAYERSEL	= $04
SM_TO_GAME		= $05

SPLASHTIME   = $03			; Seconds to wait at logo splash screen.
TMENUTIME	 = $10			; Seconds to wait at the title splash screen.
STORYTIME    = $08			; Seconds to wait on each screen of the story.

; NameTable Indexes
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

NT_SPLASH_IDX		= $00
NT_TITLE_IDX		= $01
NT_PLAYERSEL_IDX	= $02

; Cursor Palette Rotation Defines
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
PAL_FRAMECOUNT 	= $00
PAL_FRAMEDUR   	= $02
PAL_ADDR 	   	= $13

; Menu Cursor Defines
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .

CUR_FRAMECOUNT  = $0C		; Cursor frame count.
CUR_FRAMEDUR	= $03		; Cursor frame duration.
CUR_DEFAULTX	= $58		; Cursor X Location
CUR_DEFAULTY	= $6E       ; Cursor Y Location
CUR_Y0			= CUR_DEFAULTY			; 1 Player Y position
CUR_Y1			= CUR_DEFAULTY + $10	; 2 Player Y position

; Variables - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

Menu_Frames		= m0		; Menu Timer, frames until 1 second has completed.
Menu_Seconds	= m1		; Number of seconds till transition.
Menu_NextState	= m2		; Next State, indicates whether to go to story mode
							; .. or player select mode.
Pal_Timer	   	= m3        ; Palette Timer			( Unused )
Pal_Frame	   	= m4        ; Palette Frame Index   ( Unused )
Cur_Players		= m5		; Player count (0) 1 Player (1) 2 Players
Cur_Timer		= m6        ; Timer used for strobing and animation.
Cur_Frame 		= m7        ; Frame index
Cur_X			= m8        ; Cursor X Position on Screen	
Cur_Y			= m9        ; Cursor Y Position on Screen

; Player Select Defines
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
P1_Select		= mA		; Currently Selected Player 1 Character
P2_Select		= mB		; Currently Selected Player 2 Character
P1_Lock			= mC		; Player 1 Lock in Flag
P2_Lock	        = mD        ; Player 2 Lock in Flag

; Temp Values
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
Temp			= mE		; Worker Var
Temp2			= mF        ; Worker Var

; =============================================================================
; _MainMenu																	
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Select the menu mode based on ProgramMode's low nybble.
; =============================================================================
_MainMenu:
	jsr F_ClearProtected
	lda ProgramMode				; Get the SubMode we're on.
	and #$0F					; --
	asl							; Call the function vector to that mode.
	tay							;
	lda D_MenuVectors, y    	;
	sta v0						;
	lda D_MenuVectors + $01, y	;
	sta v1						; .. we use a tail return here.
	jmp (v0)					; --
	
D_MenuVectors:
.addr _InitMenu		; [0]
.addr _Splash       ; [1]
.addr _Title		; [2]
.addr _Story        ; [3]
.addr _PlayerSelect ; [4]
.addr _ToGame		; [5]

; _InitMenu																	[0]
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_InitMenu:
	jsr F_Blackout
	jsr F_WaitVblank
	jsr F_DisableGfx		; Disable Sprites / BG. We going to do some work :)
	jsr F_ResetScroll		;
	ldx #$00
	stx PpuBufferWrite
	stx PpuBufferRead
	ldy #$00                ; Clear NameTable 0 and Hide Sprites
	jsr F_ClearNameTable    ; 
	jsr F_HideSprites       ;--
	
	lda #$00				; Load NullFilter Logo NT
	ldy #NT_SPLASH_IDX      ; 
	jsr F_LoadNametable     ; --
	
	lda #$04				; Load the font tileset.
	ldy #$00                ; 
	jsr F_LoadTileset		; --
	lda #$05
	ldy #$01
	jsr F_LoadTileset		; --
	lda #$00
	ldy #$00
	jsr F_LoadTileset		; --
	
	lda #$00				; Load Palette 
	ldy #$00                ;
	jsr F_LoadPalette       ; --
	lda #$01				; Load Palette 
	ldy #$01                ;
	jsr F_LoadPalette       ; --
	lda #$02				; Load Palette 
	ldy #$02                ;
	jsr F_LoadPalette       ; --
	lda #$03				; Load Palette 
	ldy #$11                ;
	jsr F_LoadPalette       ; --
	lda #$04				; Load Palette 
	ldy #$13                ;
	jsr F_LoadPalette       ; --

	jsr F_EnableGfx			; Turn the screen back on.
	
	lda #(M_MENU + SM_SPLASH)
	sta ProgramMode
	rts

; _Splash																	[1]
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_Splash:
	lda #$04				; Fade in the screen.
	jsr F_FadeIn			; --
	
	ldy #$3C				; Set Timer for #SPLASHTIME seconds.
	ldx #SPLASHTIME			;
	lda #(M_MENU + SM_TITLE); Set next SubMode to SM_TMENU
	sta ProgramMode			;
	jsr F_StartTimer		; --
	
	lda #$04				; Fade out the screen.
	jsr F_FadeOut 			; --
	rts

; _Title																    [2]
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_Title:
;	jsr F_Blackout			; Initialize the title Screen
	jsr F_WaitVblank
	jsr F_DisableGfx		; Disable Sprites / BG.
	lda #$00				; Load Title Screen
	ldy #NT_TITLE_IDX       ; 
	jsr F_LoadNametable     ; --
	lda #$00				; Load Palette 
	ldy #$00                ;
	jsr F_LoadPalette       ; --
	lda #$01				; Load Palette 
	ldy #$01                ;
	jsr F_LoadPalette       ; --
	lda #$02				; Load Palette 
	ldy #$02                ;
	jsr F_LoadPalette       ; --
	lda #$04				; Load Palette 
	ldy #$13                ;
	jsr F_LoadPalette       ; --
	jsr F_EnableGfx
	jsr F_InitCursor		; Show the cursor.
	
	lda #$04				; Fade on in!
	jsr F_FadeIn			; --
	
	lda #$3C				; Set Main Menu duration.
	sta Menu_Frames         ;
	lda #TMENUTIME          ;
	sta Menu_Seconds        ; --
	lda #M_MENU				; Set next SubMode to M_MENU
	sta Menu_NextState		; --

@menuLoop:
	jsr F_WaitVblank		; Wait for a frame, then update frame timers.
	dec Menu_Frames         ;
	bne @checkInput         ;
	dec Menu_Seconds        ;
	beq @nextMenuState      ;
	lda #$3C                ;
	sta Menu_Frames         ; --
@checkInput:
	lda Joy1_Delta			; Get Buttons that were pressed this frame.
	and Joy1_State			; --
	tay						; Backup A in the Y register.
	and #(JOY_UP + JOY_DOWN + JOY_SELECT)	; Check for up, down or sel press.
	beq @checkStart
	
	lda Cur_Players			; Up or down was pressed, so update the ..
	eor #$01                ; Cursor's Y position
	sta Cur_Players         ;
	tax                     ;
	lda D_CursorYPos, x     ;
	sta	Cur_Y				; --
	
@checkStart:	
	tya						; Check for Start press	
	and #(JOY_START)        ; .. Move on to update the cursor if no press.
	beq @updateCursor       ; --
	
	lda #$04							; Fade out 
	jsr F_FadeOut						;
	lda #(M_MENU + SM_PLAYERSEL)        ; Set next transition and exit.
	sta Menu_NextState                  ;
	bne @nextMenuState					; --
	
@updateCursor:			
	jsr F_AnimateCursor		; Update cursor animation and repeat.
	jmp @menuLoop			; --
	
@nextMenuState:
	lda Menu_NextState		; Transfer control to the next menu state.
	sta ProgramMode			;
	rts						; --

D_CursorYPos:
.byte CUR_Y0, CUR_Y1

; _Story																    [3]
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_Story:
	lda #M_MENU
	sta ProgramMode
	rts

; _PlayerSelect															    [4]
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_PlayerSelect:
	jsr F_HideSprites		; Hide all sprites.
	jsr F_DisableGfx		; Disable Sprites / BG.
	lda #$00				; Load Title Screen
	ldy #NT_PLAYERSEL_IDX   ;
	jsr F_LoadNametable     ;--
	lda #$00				; Load Palette 
	ldy #$14                ;
	jsr F_LoadPalette       ; --
	lda #$01				; Load Palette 
	ldy #$09                ;
	jsr F_LoadPalette       ; --
	lda #$02				; Load Palette 
	ldy #$0B                ;
	jsr F_LoadPalette       ; --
	lda #$03				; Load Palette 
	ldy #$12                ;
	jsr F_LoadPalette       ; --
	jsr F_EnableGfx         ;

	lda #$00				; Set Player Select Cursor positions.
	sta P1_Select           ; Set Both players to not locked in.
	sta P1_Lock				; 
	sta P2_Lock             ;
	lda #$01                ; 
	cmp Cur_Players			; Set Player 2 Select to #$FF if in 1p mode.
	beq :+					;
	lda #$FF				;
:	sta P2_Select           ;--
	
	ldx PpuBufferWrite		; Push Attribute Update to PpuBuffer
	lda #$C9                ; .. Vram Addr
	ldy #$23          		;	 
	jsr F_PpuBufferPushWord ;
	lda #$01                ; .. Data length byte
	ldy #$AA                ; .. Attribute byte
	jsr F_PpuBufferPushWord ;
	stx PpuBufferWrite      ;--

	lda #$55				; Highlight Player 1 Attribs
	ldy P1_Select			;
	jsr F_HighlightAttrib	;--
	
	lda Cur_Players
	beq @fadeIn

	lda #$AA				; Highlight Player 2 Attribs
	ldy P2_Select			;
	jsr F_HighlightAttrib	;--
	
@fadeIn:	
	lda #$04				; All setup, lets fade in!
	jsr F_FadeIn            ; --

	
@selectLoop:				
	jsr F_WaitVblank		; Wait for a frame.
	
	lda P1_Lock				; Check to see if Player 1 locked in.
	bne @checkP2			; Check P2 if we're locked in.
	
	lda Joy1_Delta			; Check P1 input.
	and Joy1_State			;--
	beq @checkP2			; Check P2 input if P1 input is neutral.
	
	ldy P1_Select			; Update Player Quadrant
	jsr F_UpdateDirs		;
	cpy P1_Select			; Check to see if there was a change.
	beq @checkP2			; No change, continue to checkP2
	
	sty Temp2				; Backup Y

	ldy P1_Select			; Check to see if we're on the same quadrant as
	cpy P2_Select			; .. Player2 and make sure we highlight to P1 color
	bne :+					; .. if that is so. 
	lda #$AA				;
	bne :++					;
:	lda #$00				; Clear Previous location highlight.
:	jsr F_HighlightAttrib   ;--
	
	lda #$55				; Update new location highlight.
	ldy Temp2				;
	sty P1_Select			;
	jsr F_HighlightAttrib	;--
	
@checkP2:					
	lda Cur_Players			; Only do this if P2 mode is active.
	beq @checkLockin		; --

	lda P2_Lock				; Check to see if Player 1 locked in.
	bmi @checkLockin		; Check for all players ready.

	lda Joy2_Delta			; Check P1 input.
	and Joy2_State			;--
	beq @flickerCheck		; Check P2 input if P1 input is neutral.


	ldy P2_Select			; Update Player Quadrant
	jsr F_UpdateDirs		;
	cpy P2_Select			; Check to see if there was a change.
	beq @flickerCheck		; No change, continue to the flicker check.
	
	sty Temp2				; Backup Y
	
	ldy P2_Select			; Check to see if we're on the same quadrant as
	cpy P1_Select			; .. Player1 and make sure we highlight to P1 color
	bne :+					; .. if that is so. 
	lda #$55				;
	bne :++					;
:	lda #$00				; Clear Previous location highlight.
:	jsr F_HighlightAttrib   ;--
	
	lda #$AA				; Update new location highlight.
	ldy Temp2				;
	sty P2_Select			;
	jsr F_HighlightAttrib	;--
	
	
@flickerCheck:
	lda P1_Select			; Skip flicker check on 1P mode.
	cmp P2_Select           ;
	bne @checkLockin        ;--
	
	lda Rng					; Alternate between palette 1 and 2 every 8 frames.
	and #$08                ;
	cmp Cur_Frame			; Check to see if we need to update..
	beq @checkLockin		;
	sta Cur_Frame			; .. and update the flicker color if there was a 
	lda Cur_Frame			;
	bne :+                  ; .. change.
	lda #$55                ;
	bne :++                 ;
:	lda #$AA                ;
:	ldy P1_Select           ;
	jsr F_HighlightAttrib   ;--
	
@checkLockin:
	lda #$01		; Check for 2 Player mode.
	cmp Cur_Players ;
	bne :+          ;--
	and P2_Lock		; Check for Player 2 Locked in.
:	and P1_Lock		; Check for Player 1 Locked in.

	bne @exitMenu	; If both players locked in, then get out of the menu.
	
	lda P1_Lock		; Check if Player1 already locked in.
	bne :+
	lda Joy1_State	; Check for Lockin button presses.
	and Joy1_Delta  ;
	and #$F0		; If A, B, Select or Start are pressed,
	beq :+			; .. then lock in Player 2
	lda #$01		;
	sta P1_Lock     ;--

:	lda Cur_Players	; Check if we're in 2p mode.
	beq :+			; Skip this check if in 1p mode.
	
	lda P2_Lock		; Check if Player2 already locked in.
	bne :+
	lda Joy2_State	; Check for Lockin button presses.
	and Joy2_Delta	;
	and #$F0		; If A, B, Select or Start are pressed,
	beq :+			; .. then lock in Player 2
	lda #$01		;
	sta P2_Lock     ;--

:	jmp @selectLoop	; .. else loop.
	
@exitMenu:
	lda #(M_MENU + SM_TO_GAME)	; Set next SubMode to Game Mode, there's a
	sta ProgramMode				; bit of cleanup and transfer work to do there.
	rts							;--

; _ToGame																    [5]
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
_ToGame:
	lda #$04				; Fade out with 4 frames between steps.
	jsr F_FadeOut           ; --

	jsr F_WaitVblank		;
	jsr F_HideSprites		; Hide all sprites.
	jsr F_DisableGfx		; Disable Sprites / BG.
	
	lda #$00				; Cleanup the Nametable we were using.
	jsr F_ClearNameTable	;--
	
	jsr F_EnableGfx			; Enable Sprites / BG.
	
	lda P1_Select			; Set Player 1 Active and current selection.
	ora #$08				;
	sta PlayerChars			;--
	
	lda Cur_Players			; Check for 2 player mode.
	beq :+					;--
	lda P2_Select			; Set Player 2 Active and current selection.
	ora #$08				;
	asl                     ;
	asl                     ;
	asl                     ;
	asl                     ;
	ora PlayerChars         ;
	sta PlayerChars			;--
	
:	lda #$00				; Set Level-Stage = 1-1
	sta Level               ; 
	sta Stage               ;--

	lda #M_INITGAME			; Transfer Program Control to InitGame
	sta ProgramMode			;--
	
	rts

 
; =============================================================================
; F_UpdateDirs
; Updates the player select quadrant for selecting a character.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; A : Button input
; Y : Player Select Quadrant
; Returns : 
; Y : Updated Player Select Quadrant
; =============================================================================
F_UpdateDirs:
	clc
	sta Temp					; Backup button inputs.
	and #(JOY_LEFT + JOY_RIGHT)	; Check for Left or Right
	beq @checkUpDown			; Move on to Up or Down if they're clear.
	and #JOY_LEFT				; Check for Left Press
	bne :+
	tya							; Right Press
	ora #$01					;
	bne :++						;--
:	tya							; Left Press
	and #$FE					;--
:	tay
@checkUpDown:	
	lda Temp					; Restore button inputs
	and #(JOY_UP + JOY_DOWN)	; Check for Up or Down
	beq @return					; Return if clear.
	and #JOY_UP					; Check for Up Press
	bne :+
	tya							; Down Press
	ora #$02					;
	bne :++						;--
:	tya							; Up Press
	and #$FD					;--
:	tay
@return:					
	rts
; =============================================================================
; F_HighlightAttrib
;
; Highlights one of the four player select attribute areas.
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Arguments:
; A : Attribute Value
; Y : Target Attribute Location (0, 1, 2, 3)
; =============================================================================
F_HighlightAttrib:
	sta Temp
	ldx PpuBufferWrite		; Set Target VRam Address for Attrib Update
	lda D_AttribLo, y       ;
	ldy #$23                ;
	jsr F_PpuBufferPushWord ;--
	
	lda #$01				; A = Data length
	ldy Temp				; Y = Attribute Value
	jsr F_PpuBufferPushWord	;--
	stx PpuBufferWrite		; Commit PpuBuffer write.
	rts

; Pointers to VRam Attrib Low addresses.
D_AttribLo:	
.byte $C9, $CE, $E9, $EE
D_AttribValue:
.byte $00, $55, $AA

; =============================================================================
; F_InitCursor
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Initializes the Menu Cursor
; =============================================================================
F_InitCursor:
	lda #CUR_FRAMEDUR		; Set Cursor Default Values.
	sta Cur_Timer           ;
	lda #$00                ;
	sta Cur_Frame           ;
	sta Cur_Players				;
	lda #CUR_DEFAULTX		;
	sta Cur_X               ;
	lda #CUR_DEFAULTY       ;
	sta Cur_Y               ;--
	rts
	
; =============================================================================
; F_AnimateCursor
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Performs animation and timing of the Main Menu Cursor
; =============================================================================
F_AnimateCursor:
	dec Cur_Timer
	bne @return
	lda #CUR_FRAMEDUR		; Reset Frame Duration
	sta Cur_Timer			;--
	
	ldy Cur_Frame			; Update Frame Index
	iny                     ; 
	cpy #CUR_FRAMECOUNT     ;
	bne :+                  ;
	ldy #$00                ;
:	sty Cur_Frame           ;--
	
	lda Cur_Y				; Update Sprite
	sta OamRam             	;
	lda D_CursorFrames, y   ;
	sta OamRam + $01        ;
	lda #$00                ;
	sta OamRam + $02        ;
	lda Cur_X               ;
	sta OamRam + $03        ;--
@return:
	rts

D_CursorFrames:
.byte $01, $01, $01, $02, $02, $03, $04, $03, $02, $02, $01, $01

.endscope