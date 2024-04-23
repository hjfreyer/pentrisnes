; -----------------------------------------------------------------------------
;   File: nihil.s
;   Description: Your very first SNES game!
; -----------------------------------------------------------------------------

.include "mmap.s"

;----- Assembler Directives ----------------------------------------------------
.p816                           ; tell the assembler this is 65816 code
.smart
;-------------------------------------------------------------------------------

.segment "SPRITEDATA"
SpriteData: .incbin "out/sprites.vra"
SpriteData_End:
ColorData:  .incbin "out/colors.pal"
ColorData_End:


.segment "CODE"
;-------------------------------------------------------------------------------
;   This is the entry point of the demo
;-------------------------------------------------------------------------------
.proc   ResetHandler
        sei                     ; disable interrupts
        clc                     ; clear the carry flag
        xce                     ; switch the 65816 to native (16-bit mode)
        rep #$10 
        sep #$20 
        lda #$8f                ; force v-blanking
        sta INIDISP
        stz NMITIMEN            ; disable NMI

        ; jsr DMA_Tiles
        jsr DMA_Palette
        ; jsr DMA_Tilemap


        stz VMADDL              ; set the VRAM address to $0000
        stz VMADDH
        lda #$80
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH
        ldx #$00                ; set register X to zero, we will use X as a loop counter and offset
VRAMLoop:
        lda SpriteData, X       ; get bitplane 0/2 byte from the sprite data
        sta VMDATAL             ; write the byte in A to VRAM
        inx                     ; increment counter/offset
        lda SpriteData, X       ; get bitplane 1/3 byte from the sprite data
        sta VMDATAH             ; write the byte in A to VRAM
        inx                     ; increment counter/offset
        cpx #(SpriteData_End - SpriteData)                ; check whether we have written $04 * $20 = $80 bytes to VRAM (four sprites)
        bcc VRAMLoop            ; if X is smaller than $80, continue the loop
	

        .byte $42, $00          ; debugger breakpoint


	lda #$00
	sta VMADDL ; $2116 set an address in the vram of $6000
        lda #$60
	sta VMADDH ; $2116 set an address in the vram of $6000
        
        lda #$80
        sta VMAINC              ; increment VRAM address by 1 when writing to VMDATAH
        
.repeat 6, PAL
.repeat 5, SPR
        lda #SPR
        sta VMDATAL
        lda #(PAL << 2)
        sta VMDATAH
.endrep
.endrep
      
	lda #1 ; mode 1, tilesize 8x8 all
	sta BGMODE ; $2105
	
; 210b = tilesets for bg 1 and bg 2
; (210c for bg 3 and bg 4)
; steps of $1000 -321-321... bg2 bg1
	stz BG12NBA ; $210b BG 1 and 2 TILES at VRAM address $0000
	
	; 2107 map address bg 1, steps of $400... -54321yx
	; y/x = map size... 0,0 = 32x32 tiles
	; $6000 / $100 = $60
	lda #$60 ; bg1 map at VRAM address $6000
	sta BG1SC ; $2107

	lda #BG1_ON	; $01 = only bg 1 is active
	sta TM
        ; release forced blanking, set screen to full brightness
        lda #$0f
        sta INIDISP
        ; enable NMI, turn on automatic joypad polling
        lda #$81
        sta NMITIMEN

        jmp GameLoop            ; all initialization is done
.endproc

; .proc DMA_Tiles
; 	lda #1
; 	sta $4300 ; transfer mode, 2 registers 1 write
; 			  ; $2118 and $2119 are a pair Low/High
; 	lda #<VMDATAL  ; $2118
; 	sta BBAD0 ; destination, vram data
; 	ldx #.loword(SpriteData)
; 	stx $4302 ; source
; 	lda #^Tiles
; 	sta $4304 ; bank
; 	ldx #(SpriteData_End-SpriteData)
; 	stx $4305 ; length
; 	lda #1
; 	sta MDMAEN ; $420b start dma, channel 0
;         rts
; .endproc

.proc DMA_Palette
	stz CGADD ; $2121 cgram address = zero
	
	stz DMAP0 ; transfer mode 0 = 1 register write once
	lda #<CGDATA  ; $2122
	sta BBAD0 ; destination, cgram data
	ldx #.loword(ColorData)
	stx A1T0L ; source
	lda #^ColorData
	sta A1T0B ; bank
	ldx #(ColorData_End - ColorData)
	stx DAS0L ; length
	lda #1
	sta MDMAEN ; $420b start dma, channel 0
        rts
.endproc

; .proc DMA_Tilemap
;         ldx #$6000
; 	stx VMADDL ; $2116 set an address in the vram of $6000
	
; 	lda #1
; 	sta $4300 ; transfer mode, 2 registers 1 write
; 			  ; $2118 and $2119 are a pair Low/High
; 	lda #$18  ; $2118
; 	sta $4301 ; destination, vram data
; 	ldx #.loword(Tilemap)
; 	stx $4302 ; source
; 	lda #^Tilemap
; 	sta $4304 ; bank
; 	ldx #$700
; 	stx $4305 ; length
; 	lda #1
; 	sta MDMAEN ; $420b start dma, channel 0	
;         rts
; .endproc

;-------------------------------------------------------------------------------
; .include "header.asm"	

;-------------------------------------------------------------------------------
;   After the ResetHandler will jump to here
;-------------------------------------------------------------------------------
; .smart ; keep track of registers widths
.proc   GameLoop
        wai                     ; wait for NMI / V-Blank

        ; here we would place all of the game logic
        ; and loop forever

        ; writeGrid 10, 10

        jmp GameLoop
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Will be called during V-Blank
;-------------------------------------------------------------------------------
.proc   NMIHandler
        lda RDNMI               ; read NMI status, acknowledge NMI

        ; writeGrid 10, 10
        ; this is where we would do graphics update
        ; tsx                     ; save old stack pointer 
        ; pea OAMMIRROR           ; push mirror address to stack 
        ; ; jsr UpdateOAMRAM        ; update OAMRAM 
        ; txs                     ; restore old stack pointer

        rti
.endproc



;-------------------------------------------------------------------------------
;   Is not used in this program
;-------------------------------------------------------------------------------
.proc   IRQHandler
        ; code
        rti
.endproc
;-------------------------------------------------------------------------------

;-------------------------------------------------------------------------------
;   Interrupt and Reset vectors for the 65816 CPU
;-------------------------------------------------------------------------------
.segment "VECTOR"
; native mode   COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           NMIHandler, $0000,      $0000

.word           $0000, $0000    ; four unused bytes

; emulation m.  COP,        BRK,        ABT,
.addr           $0000,      $0000,      $0000
;               NMI,        RST,        IRQ
.addr           $0000,      ResetHandler, $0000
