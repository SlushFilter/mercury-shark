; =============================================================================
; Bank 3 - Game Logic
; =============================================================================
.include "defines.inc"
.include "ram-map.inc"

.autoimport + 
.define BANK_NUMBER		$03		; Bank Identifier

.segment "BANK3"
.org $8000
.byte BANK_NUMBER

.scope NS_GAME			; Set Game Namespace
.export F_EntityLogic, F_ParticleLogic

; #############################################################################
; Local Variables
; #############################################################################
Input_State = m0
Input_Held  = m1
Input_Delta = m2
Temp_Y		= m3

; #############################################################################
; Processes and Helper Functions
; #############################################################################
.include "./entities/ent_proc.asm"	; Entity Processing 
.include "./entities/particles.asm" ; Particle Processing and Logic
.include "./entities/physics.asm"	; Game physics and helpers.

; #############################################################################
; Entity Function Pointers
; #############################################################################
FP_EntityFunctions:
.addr Ent_Test		; [00]
.addr Ent_Player1   ; [01]
.addr Ent_Player2   ; [02]
.addr Ent_Zombro    ; [03]

.include "./entities/ent_test.asm"		; Test Entity
.include "./entities/ent_players.asm"	; Player Controller Entities
.include "./entities/ent_zombro.asm"	; Zombro!
.endscope