; =============================================================================
; physics.asm
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; A collection of helper functions for moving entities around and performing
; common physics tasks.
;
; Loaded into Bank3, part of the NS_GAME scope.
; =============================================================================


; Physics Defines
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
GRAVITY	= $40	; Gravity Subpixel Y Constant

; =============================================================================
; F_ApplyGravity
; 
; Applies grapvity physics to an entity's Y and SubY velocities. It will also
; clear the MOVE_UP flag at the apex of an upward movement to continue into a
; fall behavior.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; =============================================================================
F_ApplyGravity:
	bit Ent_MoveFlags	; Check to see if we're moving Up or Down.
	bvs @pullEnt		; --
	
	clc					; Apply gravity positively to accelerate downwards. 
	lda #GRAVITY        ;
	adc Ent_Vel_SubY    ;
	sta Ent_Vel_SubY	;
	lda #$00			;
	adc Ent_Vel_Y       ;
	sta Ent_Vel_Y       ;--
	rts
	
@pullEnt:
	sec					; Entity is moving up, so velocity should be decreasing
	lda Ent_Vel_SubY    ; .. because gravity.
	sbc #GRAVITY        ;
	sta Ent_Vel_SubY    ;
	lda Ent_Vel_Y       ;
	sbc #$00            ;
	sta Ent_Vel_Y       ;
	bpl @return         ;--
	
	lda #$00			; Floor the velocity at zero, this also is the apex of 
	sta Ent_Vel_Y       ; .. upward movement, so we zero out Y velocity and ..
	sta Ent_Vel_SubY    ; .. flag downward movement.
	lda Ent_MoveFlags   ;
	and #(MOVE_UP ^ $FF);
	sta Ent_MoveFlags   ;--
	
@return:	
	rts
	
; =============================================================================
; F_Collide
;
; Checks the collision tile of the specified coordinate.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Arguments :
; A : Screen to check.
; X : X Coord to check.
; Y : Y Coord to check.
; 
; Returns
;  C : Carry flag indicates a solid collision.
; mC : The Screen that was checked.
; mD : The X coordinate that was checked.
; mE : The Y coordinate that was checked.
; mF : The contents of the block collision.
; =============================================================================
F_Collide:
	sta mC
	stx mD
	sty mE
	jsr F_GetBlock		; Query the block we're testing.
						; Blocking information is stored like so :
						; .... ABCD
						; A : Top Left  (0) Clear (1) Solid 
						; B : Top Right (0) Clear (1) Solid 
						; C : Bot Left  (0) Clear (1) Solid 
						; D : Bot Right (0) Clear (1) Solid 
	sta mF				;					
	lda mE				; Top or bottom of quad?
	and #$10            ;
	bne @setBotQuad 	;
	ldy #$0C			; Set top quad
	bne @checkLeftRight
@setBotQuad:
	ldy #$03			; Set bot quad

@checkLeftRight:
	lda mD				; Check for left or right quad
	and #$10			;
	bne @setRight		;
	tya					; Set left quad
	and #$0A			;
	bne @checkCol		; .. check the collision.
@setRight:
	tya					; Set right quad
	and #$05			; .. then check the collision.
@checkCol:				; Return Carry Clear if no no collision.
	clc 				; .. otherwise return a set carry.
	and mF              ;
	beq @return         ;
	sec                 ;
@return:                ;
	rts                 ;--
; =============================================================================
; F_CheckInteract
; 
; Check the current blockmap using the current entity's X, Y and Screen
; location for an interactable tile. Fetches an argument and tile type from 
; the blockmap if the collision was positive. 
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns:
;            C : (0) No Collision (1) Collided with interactable
; ** Below returns are to be changed later when framework for doors and such is
;    up and running! :)
; 		    mF : Blockmap block value that was touched.
; Ent_FrameMod : Interactable argument, for visual confirmation.
; =============================================================================
F_CheckInteract:
	ldx Ent_X			; Check the blockmap at coordinate  X	
	lda Ent_Height		; .. Y - Half the Entity height
	lsr                 ; 
	eor #$FF            ;
	adc Ent_Y           ;
	tay                 ; 
	lda Ent_Screen      ; .. and the current screen.
	jsr F_Collide       ; .. return if we were not touching anything.
	bcc @return			; --
	
	lda mF				; If the last block we touched was solid or a platform 
	and #$F0            ; .. just ignore it and return.
	cmp #$10            ;
	beq @return         ; --
	
	lda mC				; Otherwise, fetch the argument from the last row of the
	ldx mD              ; .. blockmap and write it into the entity's FrameMod
	ldy #$FF            ; .. for visual confirmation.
	jsr F_GetBlock      ;
	sta Ent_FrameMod	; 
	lda mF				; 
	sec                 ; .. and return the block type / collision mask in A
	rts                 ; --
	
@return:
	clc					; No collision, return.
	rts					; --

; =============================================================================
; F_HAccelerate
;
; Applies  acceleration to  an entity's Vel_X and Vel_SubX up to a given maximum
; velocity.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Arguments :
; A - Acceleration Rate
; X - Maximum whole velocity
; Y - Maximum subpixel velocity
; =============================================================================
F_HAccelerate:
	clc					; Apply horizontal acceleration
	adc Ent_Vel_SubX	;
	sta Ent_Vel_SubX    ;
	lda Ent_Vel_X       ;
	adc #$00            ;
	sta Ent_Vel_X       ;--
	
	cpx Ent_Vel_X		; Cap X Velocity
	beq @capSubVel 		; .. Max = Vel_X, check for subpixel velocity
	bcs @return			; .. Max > Vel_X, return
	stx Ent_Vel_X       ; .. Max < Vel_X, reset it .. 
@capSubVel:             ; 
	cpy Ent_Vel_SubX    ; 
	bcs @return         ; .. cap subpixel velocity if needed.
	sty Ent_Vel_SubX    ;--
@return:
	rts

; =============================================================================
; F_HBrake 
;
; Applies a sort of frictional brake to an entity's Vel_X and Vel_SubX velocity.
; Clears the MOVE_HMOVE flag if the entity stopped.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Arguments :
; A - Braking force, or friction in subpixel velocity.
; C - (0) Entity Stopped (1) Entity still has velocity
; =============================================================================
F_HBrake:
	sec						; Apply the braking force to entity velocity.
	sta v0              	;
	lda Ent_Vel_SubX    	;
	sbc v0              	;
	sta Ent_Vel_SubX    	;
	lda Ent_Vel_X       	;
	sbc #$00            	;
	sta Ent_Vel_X       	;
	bcs @return         	; .. if we didnt cross 0, then return
	lda #$00            	; 
	sta Ent_Vel_X       	; .. otherwise, floor velocity at 0
	sta Ent_Vel_SubX    	;
	lda Ent_MoveFlags		;
	and #(MOVE_HMOVE ^ $FF)	; .. clear the HMove flag.
	sta Ent_MoveFlags		;
@return:                	;
	rts
	
; =============================================================================
; F_HMove 
;
; Applies a Horizontal left or right movement based on Ent direction flags and 
; velocity.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns :
; F_Collide's "m" values
; C : (0) No collision (1) Collision
; =============================================================================
F_HMove:
	lda Ent_MoveFlags
	and #MOVE_LEFT
	bne @walkLeft
	jsr F_MoveRightSolid
	rts
@walkLeft:	
	jsr F_MoveLeftSolid
	rts

; =============================================================================
; F_VMove
;
; Applies a Vertical up or down movement based on Ent direction flags and 
; velocity.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns :
; F_Collide's "m" values, Grounds an entity when landing.
; C : (0) No ground (1) Ground
; =============================================================================
F_VMove:
	lda Ent_MoveFlags
	and #MOVE_UP
	bne @moveUp
	jsr F_MoveDownSolid
	bcc @return
	
	lda Ent_MoveFlags
	ora #MOVE_GROUNDED
	sta Ent_MoveFlags

	rts
@moveUp:
	jsr F_MoveUpSolid
@return:
	rts
	
; =============================================================================
; F_MoveRightSolid
;
; Attempts to move the currently processing entity to the right by a its 
; velocity while checking the solid blockmap.
;
; If unable to move right, the entity is repositioned to the point just before 
; collision.
;
; Takes entity width and height into consideration.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns :
; C : (0) No solid collision (1) Solid collision
; *See F_Collide for additional collision info returns 
; =============================================================================
F_MoveRightSolid:
	clc
	lda Ent_SubX		; Update position
	adc Ent_Vel_SubX	;
	sta Ent_SubX        ;
	lda Ent_X           ;
	adc Ent_Vel_X       ;
	sta Ent_X           ;
	lda Ent_Screen		;
	adc #$00			;
	sta Ent_Screen		;--
	
	lda Ent_Height		; Get the midpoint of the entity height
	lsr					;
	sta v0              ;
	sec                 ;
	lda Ent_Y           ;
	sbc v0              ;
	tay                 ;--
	
	clc					; Get the target collision point.
	lda Ent_X			;
	adc Ent_Width       ;
	tax                 ;
	lda Ent_Screen      ;
	adc #$00      		;--
	
	jsr F_Collide		; Check the collision and return if there was none
	bcc @return			;
	
	lda mF				; Dont collide horizontally against platforms.
	and #$F0			;
	bne @noBonk			;--
	
	lda #$00			; .. otherwise, reset subx and veloity to #$00
	sta Ent_SubX		;
	sta Ent_Vel_X		;
	sta Ent_Vel_SubX	;
	
	lda mD				; .. snap to 16pixel grid
	and #$F0			;
	sec					;
	sbc Ent_Width		; .. reposition Ent_X
	sta Ent_X			;
	lda Ent_Screen		; .. and Ent_Screen
	sbc #$00			;
	sta Ent_Screen		;
	sec					; .. C = 1 indicating a bonk
	bcs @return			;--
@noBonk:
	clc 				; Exit with no bonk!
@return:	
	rts

; =============================================================================
; F_MoveLeftSolid
;
; Attempts to move the currently processing entity to the right by a its 
; velocity while checking the solid blockmap.
;
; If unable to move right, the entity is repositioned to the point just before 
; collision.
;
; Takes entity width and height into consideration. 
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns :
; C : (0) No solid collision (1) Solid collision
; *See F_Collide for additional collision info returns 
; =============================================================================
F_MoveLeftSolid:
	sec
	lda Ent_SubX		; Update position
	sbc Ent_Vel_SubX	;
	sta Ent_SubX        ;
	lda Ent_X           ;
	sbc Ent_Vel_X       ;
	sta Ent_X           ;
	lda Ent_Screen		;
	sbc #$00			;
	sta Ent_Screen		;--
	
	lda Ent_Height		; Get the midpoint of the entity height
	lsr					;
	sta v0              ;
	sec                 ;
	lda Ent_Y           ;
	sbc v0              ;
	tay                 ;--
	
	sec					; Get the target collision point.
	lda Ent_X			;
	sbc Ent_Width       ;
	tax                 ;
	lda Ent_Screen      ;
	sbc #$00      		;--
	
	jsr F_Collide		; Check the collision and return if there was none
	bcc @return			;
	
	lda mF				; Dont collide horizontally against "non-solids"
	and #$F0			;
	bne @noBonk			;--
	
	lda #$00			; .. otherwise, reset subx and velocity to #$00
	sta Ent_SubX		;
	sta Ent_Vel_X		;
	sta Ent_Vel_SubX	;
	
	clc					; .. snap to 16pixel grid
	lda mD				;
	adc #$10			;
	and #$F0			;
	clc					;
	adc Ent_Width		; .. reposition Ent_X
	sta Ent_X			;
	lda Ent_Screen		; .. and Ent_Screen
	adc #$00			;
	sta Ent_Screen		;
	sec					; .. C = 1 indicating a bonk.
	bcs @return			;--
@noBonk:
	clc					; No bonk this time, game!
@return:	
	rts
; =============================================================================
; F_MoveUpSolid
;
; Attempts to move the currently processing entity  up by it's velocity while
; checking the solid blockmap.
;
; If unable to move up, the entity is repositioned to the point just before 
; collision.
;
; Takes entity height into consideration.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns :
; C : (0) No solid collision (1) Solid collision
; *See F_Collide for additional collision info returns 
; =============================================================================
F_MoveUpSolid:
	sec					
	lda Ent_SubY		; Update Y position
	sbc Ent_Vel_SubY    ;
	sta Ent_SubY        ;
	lda Ent_Y           ;
	sbc Ent_Vel_Y   	;
	sta Ent_Y           ;
	sbc Ent_Height      ; .. and set Y register to check ceiling.
	tay                 ;--
	
	ldx Ent_X			; Set X and A to Ent_X and Ent_Screen
	lda Ent_Screen		;
	jsr F_Collide		; .. do the collision check.
	bcc @return			; .. return if no collision.
	
	lda mF 				; Check for a platform and ignore the collision 
	and #$F0			; .. if it is a non-solids.
	bne @noBonk			;--
	
	and #$10
	lda #$00			; .. clear SubY and Y velocity
	sta Ent_SubY        ;
	sta Ent_Vel_Y       ;
	sta Ent_Vel_SubY    ;--
	
	clc					; .. snap to 16pixel grid 
	lda mE              ;
	adc #$10            ;
	and #$F0            ;
	adc Ent_Height      ; .. offset final position by height.
	sta Ent_Y			;
	sec 				; .. C = 1 indicating a bonk.
	bcs @return			;--
@noBonk:
	clc					; We didn't bonk so ..
@return:				; .. return
	rts					
; =============================================================================
; F_MoveDownSolid
;
; Attempts to move the currently processing entity down by it's velocity while
; checking the solid blockmap.
;
; If unable to move down, the entity is repositioned to the point just before 
; collision.
;
; Takes entity height into consideration.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns :
; C : (0) No solid collision (1) Solid collision
; *See F_Collide for additional collision info returns 
; =============================================================================
F_MoveDownSolid:
	clc					
	lda Ent_SubY		; Update Y position
	adc Ent_Vel_SubY    ;
	sta Ent_SubY        ;
	lda Ent_Y           ;
	sta Temp_Y			; .. backup old Y position for platform checks.
	adc Ent_Vel_Y    	;
	sta Ent_Y           ;
	adc #$01			; .. and set Y register to check for a floor.
	tay                 ;--
	
	ldx Ent_X			; Set X and A to Ent_X and Ent_Screen
	lda Ent_Screen		;
	jsr F_Collide		; .. do the collision check.
	bcc @return			; .. return if no collision.
	
	lda mF				; Check for platform special handling
	and #$F0            ;  
	beq @bonk           ; .. handle normal solid
	cmp #$10            ; 
	beq @bonkPlatform	; .. handle a platform solid
	clc					;
	bcc @return			; .. otherwise, there was no collision.
	
@bonkPlatform:	
	lda mE				; Platform handling; if only collide if Y was above the
	and #$F0            ; ..platform prior to the move.
	cmp Temp_Y          ;
	bcc @return         ;--
	
@bonk:
	lda #$00			; .. clear SubY and Y velocity
	sta Ent_SubY        ;
	sta Ent_Vel_Y       ;
	sta Ent_Vel_SubY    ;--
	
	sec					; .. snap to 16pixel grid 
	lda mE              ;
	and #$F0            ;
	sbc #$01      		; .. offset final position by 1 pixel.
	sta Ent_Y			;
	sec 				; .. C = 1 indicating a bonk.
@return:				; .. return
	rts					
	
; =============================================================================
; F_CheckGround
;
; Checks for ground directly under the entity's feet.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Returns :
; Sets Entity MOVE_GROUNDED flag appropriately.
; C : (0) No ground (1) Ground
; *See F_Collide for additional collision info returns 
; =============================================================================
F_CheckGround:
	lda Ent_Screen				; Get the point to check for ground against.
	ldx Ent_X           		;
	ldy Ent_Y           		;
	iny                 		;
	jsr F_Collide				; .. check the collision
	bcc @clearGrounded			;
	
	lda mF
	and #$F0
	beq @grounded
	cmp #$10
	bne @clearGrounded
	
@grounded:
	lda Ent_MoveFlags			;
	ora #MOVE_GROUNDED			; .. flag grounded if on the ground.
	bne @return			        ;
@clearGrounded:					;
	lda Ent_MoveFlags			;
	and #MOVE_GROUNDED ^ $FF	; .. otherwise clear grounded.
@return:                		;
	sta Ent_MoveFlags   		; .. commit the flag change.
	rts							; --

; =============================================================================
; F_BoxBoxTest
;
; Checks to see if two AABB are overlapping.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Arguments :
; ox = $00	: Other X
; oy = $01	: Other Y
; ow = $02	: Other Half-Width
; oh = $03	: Other Full-Height
; sx = $04	: Self X
; sy = $05	: Self Y
; sw = $06	: Self Half-Width
; sh = $07	: Self Full-Height
;
; Returns :
; C : (0) No Collision (1) Collision
; =============================================================================
ox = $00	; Other X
oy = $01	; Other Y
ow = $02	; Other Half-Width
oh = $03	; Other Full-Height
sx = $04	; Self X
sy = $05	; Self Y
sw = $06	; Self Half-Width
sh = $07	; Self Full Height
F_BoxBoxTest:
	clc			; Calculate total width of both AABB's
	lda sw      ;
	adc ow      ;
	sta ow      ;
	clc         ;
	lda sh      ; Calculate total height of both AABB's
	adc oh      ;
	lsr         ;
	sta oh      ; .. fall through to F_PointBoxTest to complete.

; =============================================================================
; F_PointBoxTest
;
; Checks to see if a point is contained in the defined bounding box and returns
; C = 1 if they are in collision.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Arguments :
; ox = $00	: Other X
; oy = $01	: Other Y
; ow = $02	: Other Half-Width
; oh = $03	: Other Full-Height
; sx = $04	: Self X
; sy = $05	: Self Y
;
; Returns :
; C : (0) No Collision (1) Collision
; =============================================================================

F_PointBoxTest:
	sec					; x1 = ox - w
	lda ox              ;
	sbc ow              ;  
	cmp sx              ; px1 < x1 ? return C=0 : continue
	bcc @testX2     	;
	clc					;
	rts                 ;
	
@testX2:	
	clc					; x2 = ox + w
	lda ox              ;
	adc ow              ;  
	cmp sx              ; px1 > x2 ? return C=0 : continue
	bcs @testY1     	;
	rts                 ; 
	
@testY1:
	sec					; y1 = oy - h
	lda oy              ;
	sbc oh              ;  
	cmp sy              ; py1 < y1 ? return C=0 : continue
	bcc @testY2     	;
	clc					;
	rts                 ;
	
@testY2:	
	clc					; x2 = ox + w
	lda oy              ;
	adc oh              ;  
	cmp sy              ; px1 > x2 ? C=0 : C=1
	rts                 ; 


; =============================================================================
; F_BoxEnemyTest
; 
; When I first arrived at UNI, I had to admit I felt a little lost. Even though
; it was a liberating feeling, being away from home and the hum-drum drums of 
; the high school marching band, I couldn't help but still feel a little trapped.
;
; But it wasn???t for that experience I wouldn???t have spent that gap year 
; horseback riding. And honestly, if it wasnt for my horse I wouldn't have spent
; that year in college.
; 
; One day while breaking in a new horse I had the sudden urge to figure out if a 
; sword would hit it or not. But I didn't really know much about making swords 
; hit stuff. I mostly knew about riding horses. That day, as I rode around the
; yard,  I opened up my super cool laptop replica version of the good old 
; Commodore 64, and banged out this little piece of code.
; 
; Really, it answered all my questions about whether a sword would hit the horse
; or not if I were a 2D knight dressed in diamond armor.
;
; Performs hitscans against enemy entities. If the entity is inside of the 
; hitscan box passed in the arguments, the Entity's Ent_Damaged value is set to
; the value passed in the Y register.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . .
; Arguments :
; sx = $04	: Self X
; sy = $05	: Self Y
; sw = $06	: Self Half-Width
; sh = $07	: Self Full-Height
; Y 		: Damage Type to assign to a hit entity.
; =============================================================================
F_BoxEnemyTest:
	ldx #$40 					; Set Entity Index to Slot 3 ($400, X)
	
@checkEnt:
	lda Ent_Data, x				; Check if Entity is active
	bpl @nextEnt				;
								
	lda EntData_X, x			; Set F_BoxBoxTest arguments ..	
	sta ox                      ;
	lda EntData_Y, x            ;
	sta oy                      ;
	lda EntData_Width, x        ;
	sta ow                      ;
	lda EntData_Height, x       ;
	sta oh                      ;
	jsr F_BoxBoxTest            ; .. call F_BoxBoxTest
	bcc @nextEnt                ;
	tya                         ;
	sta EntData_Damaged, x 		; .. set Damaged type if hit.
					
	@nextEnt:					; Fetch the next entity ..
	clc							;
	txa                         ;
	adc #$20                    ;
	tax							;
	bne @checkEnt               ; .. return if done.
	rts							;

; =============================================================================
; F_EnemyPlayerTest
;
; Tests an enemy hitbox against a player hitbox. The result of the collision 
; will store Y into a player's Ent_Damaged field for later processing.
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
; Arguments :
; Y 		: Damage Type to assign to a hit entity.
; =============================================================================
F_EnemyPlayerTest:
	lda Ent_X						; Init worker variables, this could be made
	sta sx                          ; .. faster if we just use the Ent_xxx 
	lda Ent_Y                       ; .. values of the AABB test instead.
	sta sy                          ;
	lda Ent_Width                   ;
	sta sw                          ;
	lda Ent_Height                  ;
	sta sh                          ; --
	
	lda Ent_P1_Flags				; Only check collision if the player is 
	and #ENT_ACTIVE					; active and not in iFrames
	beq @checkP2                    ; 
	lda Ent_P1_IFrames				;
	bne @checkP2                    ; --
	
	lda Ent_P1_X					; Fetch Player 1's position and size.
	sta ox                          ;
	lda Ent_P1_Y                    ;
	sta oy                          ;
	lda Ent_P1_Width                ;
	sta ow                          ;
	lda Ent_P1_Height               ;
	sta oh                          ;
	jsr F_BoxBoxTest                ; .. perform the collision check.
	bcc @checkP2                    ; --
	
	lda sx							; Set direction flag to indicate what side 
	cmp ox                  		; the player was hit on.
	lda #$00						;
	ror 							; Move the carry flag into bit7
	and #$80                		;
	sta Ent_P1_Damaged      		;
	tya                     		; Set damage type and amount
	ora Ent_P1_Damaged      		;
	sta Ent_P1_Damaged      		;--
	
@checkP2:
	lda Ent_P2_Flags				; Only check collision if the player is
	and #ENT_ACTIVE					; active and not in iFrames
	ora Ent_P2_IFrames				;
	beq @return                     ;
	lda Ent_P2_IFrames				;
	bne @return						; --
	
	lda Ent_P2_X					; Fetch Player 2's position and size.
	sta ox                          ;
	lda Ent_P2_Y                    ;
	sta oy                          ;
	lda Ent_P2_Width                ;
	sta ow                          ;
	lda Ent_P2_Height               ;
	sta oh                          ;
	jsr F_BoxBoxTest                ; .. perform the collision check.
	bcc @return                     ; --
	
	lda sx							; Set direction flag to indicate what side
	cmp ox                  		; the player was hit on.
	lda #$00						;
	ror 							; Move the carry flag into bit7
	and #$80                		;
	sta Ent_P2_Damaged      		;
	tya                     		; Set damage type and amount
	ora Ent_P2_Damaged      		;
	sta Ent_P2_Damaged      		;--
	
@return:
	rts
	

; =============================================================================
; F_ApplyRecoil
;
; Aplies a default knockback velocity of X:1, Y:1
; Clears MOVE_GROUNDED for launching the entity.
; Sets MOVE_LEFT or clears MOVE_LEFT depending on the damage direction flag of
; Ent_Damaged 
; . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
; Arguments :
; Y 		: Damage Type to assign to a hit entity.
; =============================================================================
F_ApplyRecoil:
	lda #$01					; Set default recoil to X:1, Y:2
	sta Ent_Vel_X               ;
	lda #$00					;
	sta Ent_Vel_SubX            ;

	lda #$03					;
	sta Ent_Vel_Y               ;
	lda #$40					;
	sta Ent_Vel_SubY			;
	lda Ent_Damaged             ; .. roll the damage direciton flag into C
	rol                         ;
	bcc @recoilRight            ; --
	
	lda Ent_MoveFlags           ; Handle a knockback to the left.
	ora #MOVE_LEFT | MOVE_UP    ;
	and #MOVE_GROUNDED ^ $FF    ;
	sta Ent_MoveFlags			; --
	rts
@recoilRight:
	lda Ent_MoveFlags						; Handle a knockback to the right.
	ora #MOVE_UP                            ;
	and #(MOVE_LEFT | MOVE_GROUNDED)^ $FF	;
	sta Ent_MoveFlags						;
	rts                                     ; --
	
	
	
	
	
	
