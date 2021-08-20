; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
; NES 2.0 ROM Header       
; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
.segment "INES"

; Offset Meaning
; --------------
; 0-3    Identification String. Must be "NES<EOF>".
.byte "NES", $1A		

; 4      PRG-ROM size LSB
.byte $08				

; 5      CHR-ROM size LSB
.byte $00				

; 
; 6      Flags 6
;        D~7654 3210
;          ---------
;          NNNN FTBM
;          |||| |||+-- Hard-wired nametable mirroring type
;          |||| |||     0: Horizontal or mapper-controlled
;          |||| |||     1: Vertical
;          |||| ||+--- "Battery" and other non-volatile memory
;          |||| ||      0: Not present
;          |||| ||      1: Present
;          |||| |+--- 512-byte Trainer
;          |||| |      0: Not present
;          |||| |      1: Present between Header and PRG-ROM data
;          |||| +---- Hard-wired four-screen mode
;          ||||        0: No
;          ||||        1: Yes
;          ++++------ Mapper Number D0..D3
.byte $21	; Mapper 002 (UNROM), Vertical Nametable Mirroring

; 
; 7      Flags 7
;        D~7654 3210
;          ---------
;          NNNN 10TT
;          |||| ||++- Console type
;          |||| ||     0: Nintendo Entertainment System/Family Computer
;          |||| ||     1: Nintendo Vs. System
;          |||| ||     2: Nintendo Playchoice 10
;          |||| ||     3: Extended Console Type
;          |||| ++--- NES 2.0 identifier
;          ++++------ Mapper Number D4..D7
.byte $08	; 

; 8      Mapper MSB/Submapper
;        D~7654 3210
;          ---------
;          SSSS NNNN
;          |||| ++++- Mapper number D8..D11
;          ++++------ Submapper number
.byte $00

; 9      PRG-ROM/CHR-ROM size MSB
;        D~7654 3210
;          ---------
;          CCCC PPPP
;          |||| ++++- PRG-ROM size MSB
;          ++++------ CHR-ROM size MSB
.byte $00

; 10     PRG-RAM/EEPROM size
;        D~7654 3210
;          ---------
;          pppp PPPP
;          |||| ++++- PRG-RAM (volatile) shift count
;          ++++------ PRG-NVRAM/EEPROM (non-volatile) shift count
;        If the shift count is zero, there is no PRG-(NV)RAM.
;        If the shift count is non-zero, the actual size is
;        "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.
.byte $00
 
; 11     CHR-RAM size
;        D~7654 3210
;          ---------
;          cccc CCCC
;          |||| ++++- CHR-RAM size (volatile) shift count
;          ++++------ CHR-NVRAM size (non-volatile) shift count
;        If the shift count is zero, there is no CHR-(NV)RAM.
;        If the shift count is non-zero, the actual size is
;        "64 << shift count" bytes, i.e. 8192 bytes for a shift count of 7.
.byte $07	; 8k of CHR-RAM
 
; 12     CPU/PPU Timing
;        D~7654 3210
;          ---------
;          .... ..VV
;                 ++- CPU/PPU timing mode
;                      0: RP2C02 ("NTSC NES")
;                      1: RP2C07 ("Licensed PAL NES")
;                      2: Multiple-region
;                      3: UMC 6527P ("Dendy")
.byte $00	; NTSC NES
 
; 13     When Byte 7 AND 3 =1: Vs. System Type
;        D~7654 3210
;          ---------
;          MMMM PPPP
;          |||| ++++- Vs. PPU Type
;          ++++------ Vs. Hardware Type
; 
;        When Byte 7 AND 3 =3: Extended Console Type
;        D~7654 3210
;          ---------
;          .... CCCC
;               ++++- Extended Console Type
.byte $00	; Standard NES

; 14     Miscellaneous ROMs
;        D~7654 3210
;          ---------
;          .... ..RR
;                 ++- Number of miscellaneous ROMs present
.byte $00	; Standard NES

; 15     Default Expansion Device
;        D~7654 3210
;          ---------
;          ..DD DDDD
;            ++-++++- Default Expansion Device
.byte $01	; Standard NES controllers