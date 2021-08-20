; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Bank 1 - Sprite CHR Data
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.export DP_SpriteSets

.define BANK_NUMBER		$01		; Bank Identifier

.segment "BANK1"
.org $8000

.byte BANK_NUMBER
; =============================================================================
; DP_SpriteSets :
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; Pointers to sprite chr data.
; =============================================================================

DP_SpriteSets:
.addr CHR_MENUSPR   ; [00] Menu Sprites
.addr CHR_DKNIGHT	; [01] Diamond Knight
.addr CHR_WEYRL     ; [02] Weyrl
.addr CHR_VIPER     ; [03] Viper*
.addr CHR_ERANGER   ; [04] Ember Ranger
.addr CHR_CSPRITES	; [05] Common Sprites
.addr CHR_MONSTERS  ; [06] Agh! Monsters! 
CHR_MENUSPR:
.incbin "chr-data/menu-sprites.chr"
CHR_DKNIGHT:
.incbin "chr-data/d-knight.chr"
CHR_WEYRL:
.incbin "chr-data/weyrl.chr"
CHR_VIPER:
.incbin "chr-data/viper.chr"
CHR_ERANGER:
.incbin "chr-data/e-ranger.chr"
CHR_CSPRITES:
.incbin "chr-data/csprites.chr"
CHR_MONSTERS:
.incbin "chr-data/zombro.chr"